package io.choerodon.agile.infra.config;

import java.time.Duration;
import java.util.concurrent.Executor;
import java.util.concurrent.ThreadPoolExecutor;

import org.modelmapper.ModelMapper;
import org.modelmapper.convention.MatchingStrategies;
import org.springframework.beans.factory.SmartInitializingSingleton;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.cache.CacheManager;
import org.springframework.cache.annotation.CachingConfigurerSupport;
import org.springframework.cache.interceptor.KeyGenerator;
import org.springframework.cloud.openfeign.EnableFeignClients;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.redis.cache.RedisCacheConfiguration;
import org.springframework.data.redis.cache.RedisCacheManager;
import org.springframework.data.redis.cache.RedisCacheWriter;
import org.springframework.data.redis.connection.RedisConnectionFactory;
import org.springframework.data.redis.core.HashOperations;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.ValueOperations;
import org.springframework.data.redis.serializer.GenericJackson2JsonRedisSerializer;
import org.springframework.data.redis.serializer.JdkSerializationRedisSerializer;
import org.springframework.data.redis.serializer.StringRedisSerializer;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.security.concurrent.DelegatingSecurityContextExecutorService;

import io.choerodon.agile.infra.aspect.MessageAspect;
import io.choerodon.core.config.async.ChoerodonTaskDecorator;
import io.choerodon.core.config.async.plugin.ChoerodonTaskDecoratorPlugin;

import org.hzero.core.message.MessageAccessor;

/**
 * 敏捷服务自动配置
 * @author gaokuo.dai@zknow.com 2023-06-07
 */
@Configuration
@EnableFeignClients("io.choerodon")
@EnableConfigurationProperties(AgileServiceConfigurationProperties.class)
public class AgileServiceConfiguration {

    /**
     * @return 多语言消息目录
     */
    @Bean
    public SmartInitializingSingleton agileMessagesLoader() {
        return () -> {
            MessageAccessor.addBasenames("classpath:messages/messages_agile");
        };
    }

    @Autowired
    private ChoerodonTaskDecorator choerodonTaskDecorator;

    /**
     * @return 工作列表导入线程池
     */
    @Bean
    public Executor issueImportExecutor() {
        ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
        executor.setCorePoolSize(4);
        executor.setMaxPoolSize(16);
        executor.setQueueCapacity(200);
        executor.setThreadNamePrefix("issueImportExecutor-");
        executor.setTaskDecorator(choerodonTaskDecorator);
        executor.initialize();
        return new DelegatingSecurityContextExecutorService(executor.getThreadPoolExecutor());
    }

    /**
     * @return ModelMapper
     */
    @Bean
    public ModelMapper modelMapper() {
        ModelMapper modelMapper = new ModelMapper();
        modelMapper.getConfiguration().setMatchingStrategy(MatchingStrategies.STRICT);
        return modelMapper;
    }

    /**
     * @return async注解线程插件
     */
    @Bean
    public ChoerodonTaskDecoratorPlugin<Boolean> choerodonSendMessageFlagTaskDecoratorPlugin() {
        return new ChoerodonTaskDecoratorPlugin<Boolean>() {
            @Override
            public Boolean getResource() {
                return MessageAspect.SEND_MSG_FLAG.get();
            }

            @Override
            public void setResource(Boolean resource) {
                MessageAspect.SEND_MSG_FLAG.set(resource);
            }
        };
    }

    /**
     * redis配置
     */
    @Configuration
    public static class RedisCacheConfig extends CachingConfigurerSupport {

        /**
         * 异步方法删除redis缓存的线程池
         */
        @Bean("redisTaskExecutor")
        public Executor taskExecutor() {
            ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
            executor.setCorePoolSize(5);
            executor.setMaxPoolSize(50);
            executor.setQueueCapacity(1000);
            executor.setKeepAliveSeconds(60);
            executor.setThreadNamePrefix("delete-redis-cache-");
            executor.setRejectedExecutionHandler(new ThreadPoolExecutor.CallerRunsPolicy());
            return executor;
        }

        /**
         * 配置redisTemplate<br/>
         * 通过redisConnectionFactory引入redis在配置文件中的连接配置
         */
        @Bean
        public RedisTemplate<String, Object> redisTemplate(RedisConnectionFactory redisConnectionFactory) {
            RedisTemplate<String, Object> redisTemplate = new RedisTemplate<>();
            initDomainRedisTemplate(redisTemplate, redisConnectionFactory);
            return redisTemplate;
        }

        /**
         * 设置数据存入 redis 的序列化方式
         *
         * @param redisTemplate redisTemplate
         * @param factory       factory
         */
        private void initDomainRedisTemplate(RedisTemplate<String, Object> redisTemplate, RedisConnectionFactory factory) {
            redisTemplate.setKeySerializer(new StringRedisSerializer());
            redisTemplate.setValueSerializer(new JdkSerializationRedisSerializer());
            redisTemplate.setHashKeySerializer(new StringRedisSerializer());
            redisTemplate.setHashValueSerializer(new GenericJackson2JsonRedisSerializer());
            redisTemplate.setConnectionFactory(factory);
        }

        /**
         * 实例化 HashOperations 对象,可以使用 Hash 类型操作
         *
         * @param redisTemplate redisTemplate
         * @return HashOperations
         */
        @Bean
        public HashOperations<String, String, Object> hashOperations(RedisTemplate<String, Object> redisTemplate) {
            return redisTemplate.opsForHash();
        }

        /**
         * 实例化 ValueOperations 对象,可以使用 String 操作
         *
         * @param redisTemplate redisTemplate
         * @return ValueOperations
         */
        @Bean
        public ValueOperations<String, Object> valueOperations(RedisTemplate<String, Object> redisTemplate) {
            return redisTemplate.opsForValue();
        }

        @Bean
        public CacheManager cacheManager(RedisConnectionFactory redisConnectionFactory) {
            RedisCacheConfiguration redisCacheConfiguration = RedisCacheConfiguration.defaultCacheConfig()
                    .entryTtl(Duration.ofHours(24)); // 设置缓存有效期一小时
            return RedisCacheManager
                    .builder(RedisCacheWriter.nonLockingRedisCacheWriter(redisConnectionFactory))
                    .cacheDefaults(redisCacheConfiguration).transactionAware().build();
        }

        /**
         * redis的key生成算法重写
         */
        @Bean
        public KeyGenerator customKeyGenerator() {
            return (o, method, objects) -> {
                StringBuilder sb = new StringBuilder();
                sb.append(o.getClass().getName());
                sb.append(method.getName());
                for (Object obj : objects) {
                    sb.append(obj.toString());
                }
                return sb.toString();
            };
        }
    }

}
