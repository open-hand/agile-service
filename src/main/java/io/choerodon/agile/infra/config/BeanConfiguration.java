package io.choerodon.agile.infra.config;

import io.choerodon.agile.infra.utils.CommonMapper;
import org.hzero.core.jackson.config.ObjectMapperPostProcess;
import org.modelmapper.ModelMapper;
import org.modelmapper.convention.MatchingStrategies;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * @author shinan.chen
 * @date 2018/9/27
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
    public CommonMapper commonMapper(ObjectMapperPostProcess objectMapperPostProcess){
        return (CommonMapper) objectMapperPostProcess.postProcessAfterInitialization(new CommonMapper(), "commonMapper");
    }

}