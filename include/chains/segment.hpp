/*
    Copyright 2026 Adobe
    Distributed under the Boost Software License, Version 1.0.
    (See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
*/

#ifndef CHAINS_SEGMENT_HPP
#define CHAINS_SEGMENT_HPP

#include <chains/config.hpp>
#include <chains/tuple.hpp>

#include <exception>
#include <tuple>
#include <utility>

namespace chains::inline CHAINS_VERSION_NAMESPACE() {

template <class T>
struct type {};

template <class Injects, class Applicator, class... Fs>
class segment {
    std::tuple<Fs...> _functions;
    Applicator _apply;

public:
    /*
        An apply operation may inject additional arguments into the segment. The plan is that the
        receiver will get sent to apply and this is how cancellation tokens can be injected into an
        operation. Something like `with_cancellation`.

        This feature is also used for the `then` operation where the resolve future is injected into
        the segment.
    */
    template <class... Args>
    auto result_type_helper(Args&&... args) && {
        return interpret(std::move(_functions))(std::forward<Args>(args)...);
    }

    explicit segment(type<Injects>, Applicator&& apply, std::tuple<Fs...>&& functions)
        : _functions{std::move(functions)}, _apply{std::move(apply)} {}

    explicit segment(type<Injects>, Applicator&& apply, Fs&&... functions)
        : _functions{std::move(functions)...}, _apply{std::move(apply)} {}

    /*
        The basic operations should follow those from C++ lambdas, for now default everything.
        and see if the compiler gets it correct.
    */
    segment(const segment&) = default;
    segment(segment&&) noexcept = default;
    auto operator=(const segment&) -> segment& = default;
    auto operator=(segment&&) noexcept -> segment& = default;

    template <class F>
    auto append(F&& f) && {
        return chains::segment{
            type<Injects>{}, std::move(_apply),
            std::tuple_cat(std::move(_functions), std::tuple{std::forward<F>(f)})};
    }

    /*
        The apply function for a segment always returns void.

        Invoke will check the receiver for cancellation -
        If not canceled, apply(segment), cancellation is checked before execution of the segment
        and any exception during the segment is propagated to the receiver.
    */

    template <class R, class... Args>
    auto invoke(R&& receiver, Args&&... args) && {
        // TODO: must handle this cancel prior to invoking the segment.
        // if (receiver.canceled()) return;
        return std::move(_apply)(
            [_f = interpret(std::move(_functions)),
             _receiver = std::forward<R>(receiver)]<typename... T>(T&&... args) mutable noexcept {
                if (_receiver->canceled()) return;
                try {
                    std::move(_f)(std::forward<T>(args)...);
                } catch (...) {
                    _receiver->set_exception(std::current_exception());
                }
            },
            std::forward<Args>(args)...);
    }
};

} // namespace chains::inline CHAINS_VERSION_NAMESPACE()

#endif
