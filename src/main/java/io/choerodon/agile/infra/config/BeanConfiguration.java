package io.choerodon.agile.infra.config;

import org.modelmapper.ModelMapper;
import org.modelmapper.convention.MatchingStrategies;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import io.choerodon.agile.infra.aspect.MessageAspect;
import io.choerodon.core.config.async.plugin.ChoerodonTaskDecoratorPlugin;

/**
 * @author shinan.chen 2018/9/27
 */
@Configuration
public class BeanConfiguration {

    @Bean
    public ModelMapper modelMapper() {
        ModelMapper modelMapper = new ModelMapper();
        modelMapper.getConfiguration().setMatchingStrategy(MatchingStrategies.STRICT);
        return modelMapper;
    }

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
}
