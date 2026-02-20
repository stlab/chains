/*
Copyright 2026 Adobe
  Distributed under the Boost Software License, Version 1.0.
  (See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
*/

#ifndef CHAINS_SYNC_WAIT_HPP
#define CHAINS_SYNC_WAIT_HPP

#include <chains/config.hpp>

namespace chains {
inline namespace CHAINS_VERSION_NAMESPACE() {

template <class Chain, class... Args>
auto sync_wait(Chain&& chain, Args&&... args) {
    using result_t = typename Chain::template result_type<Args...>;

    struct receiver_t {
        std::optional<result_t> result;
        std::exception_ptr error{nullptr};
        std::mutex m;
        std::condition_variable cv;

        void operator()(result_t&& value) {
            {
                std::lock_guard lock(m);
                result = std::move(value);
            }
            cv.notify_one();
        }

        void set_exception(std::exception_ptr p) {
            {
                std::lock_guard lock(m);
                error = p;
            }
            cv.notify_one();
        }

        bool canceled() const { return false; }
    };

    /*
       REVISIT: (sean-parent) - chain invoke doesn't work with std::ref(receiver). We should
       fix that but for now create a receiver-ref.
    */

    auto receiver = std::make_shared<receiver_t>();

    std::forward<Chain>(chain).invoke(receiver, std::forward<Args>(args)...);

    std::unique_lock<std::mutex> lock(receiver->m);
    receiver->cv.wait(lock, [&] { return receiver->result.has_value() || receiver->error; });

    if (receiver->error) {
        std::rethrow_exception(receiver->error);
    }
    return *receiver->result;
}

/*
    TODO: The ergonomics of chains are painful with three arguments. We could reduce to a
   single argument or move to a concept? Here I really want the forward reference to be an
   rvalue ref.

    The implementation of sync_wait is complicated by the fact that the promise is currently
   hard/ wired into the chain. sync_wait needs to be able to invoke the promise/receiver -
   _then_ flag the condition that it is ready.
*/

} // namespace CHAINS_VERSION_NAMESPACE()
} // namespace chains

#endif