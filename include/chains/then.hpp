/*
Copyright 2026 Adobe
  Distributed under the Boost Software License, Version 1.0.
  (See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
*/

#ifndef CHAINS_THEN_HPP
#define CHAINS_THEN_HPP

#include <chains/config.hpp>
#include <chains/chains.hpp>

#include <type_traits>
#include <utility>

namespace chains {
inline namespace CHAINS_VERSION_NAMESPACE() {


    /*
        The `then` algorithm takes a future and returns a segment (chain) that will schedule the
        segment as a continuation of the future.

        The segment returns void so the future is (kind of) detached - but this should be done
        without the overhead of a future::detach.

        How is cancellation handled here? Let's say we have this:

        `auto f = start(then(future));`

        And we destruct f. We need to _delete_ the (detached) future. Where is this held? f is only
        holding the promise.
    */

    template <class F>
    auto then(F&& future) {
        return chain{std::tuple<>{},
                     segment{type<typename std::decay_t<F>::result_type>{},
                             [_future = std::forward<F>(future)]<typename C>(C&& continuation) mutable {
                                 return std::move(_future).then(std::forward<C>(continuation));
                             }}};
    }

    // TODO: (sean-parent) - should we make this pipeable?
    // TODO: (sean-parent) - fix case where invoke_t is void.


}
} // namespace chains

#endif