/*
Copyright 2026 Adobe
  Distributed under the Boost Software License, Version 1.0.
  (See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
*/

#ifndef CHAINS_START_HPP
#define CHAINS_START_HPP

#include <chains/config.hpp>

#include <stlab/concurrency/await.hpp>
#include <stlab/concurrency/future.hpp>

namespace chains {
inline namespace CHAINS_VERSION_NAMESPACE() {

template <class Chain, class... Args>
inline auto start(Chain&& chain, Args&&... args) {
    using result_t = Chain::template result_type<Args...>;
    using package_task_t = stlab::packaged_task<result_t>;

    using invoke_t = decltype(std::forward<Chain>(chain).invoke(
        std::declval<std::shared_ptr<stlab::packaged_task<result_t>>>(),
        std::forward<Args>(args)...));

    auto shared_receiver = std::shared_ptr<package_task_t>();

    if constexpr (std::is_same_v<invoke_t, void>) {
        auto [receiver, future] = stlab::package<result_t(result_t)>(
            stlab::immediate_executor, []<typename T>(T&& val) { return std::forward<T>(val); });

        // Promote receiver to shared_ptr to extend lifetime beyond this scope and circumvent the
        // move only capabilities of package_task.
        shared_receiver = std::make_shared<package_task_t>(std::move(receiver));

        std::forward<Chain>(chain).invoke(std::move(shared_receiver), std::forward<Args>(args)...);

        return std::move(future);
    } else {
        auto p = std::make_shared<std::optional<invoke_t>>();
        auto [receiver, future] = stlab::package<result_t(result_t)>(
            stlab::immediate_executor,
            [p]<typename V>(V&& value) { return std::forward<V>(value); });

        shared_receiver = std::make_shared<package_task_t>(std::move(receiver));

        *p = std::forward<Chain>(chain).invoke(std::move(shared_receiver),
                                               std::forward<Args>(args)...);
        return std::move(future);
    }
}

} // namespace CHAINS_VERSION_NAMESPACE()
} // namespace chains

#endif