package io.choerodon.agile.infra.utils;

import java.io.IOException;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JavaType;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.type.TypeFactory;
import io.choerodon.core.exception.CommonException;
import org.hzero.core.jackson.config.ObjectMapperPostProcess;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * objectMapper 工具类
 * 本工具类所使用objectMapper不含有加密模块,其余模块与hzero保持一致。
 * 若有更改objectMapper的需要，请在本工具类内配置，不要修改暴露出去的objectMapper,便于维护。
 * @author jiaxu.cui@hand-china.com 2020/9/23 下午1:40
 */
public class CommonMapperUtil {
    public static final Logger log = LoggerFactory.getLogger(CommonMapperUtil.class);
    
    private static final  ObjectMapper objectMapper;
    
    static {
        objectMapper = (ObjectMapper)new ObjectMapperPostProcess()
                .postProcessAfterInitialization(new ObjectMapper(), "commonMapperUtil");
    }
    
    public static String writeValueAsString(Object value){
        try {
            return objectMapper.writeValueAsString(value);
        } catch (JsonProcessingException e) {
            log.error("json serialize failed");
            throw new CommonException(e);
        }
    }
    
    public static <T> T readValue(String str, Class<T> clazz){
        try {
            return objectMapper.readValue(str, clazz);
        } catch (IOException e) {
            log.error("json deserialize failed");
            throw new CommonException(e);
        }
    }

    public static <T> T readValue(String content, JavaType valueType){
        try {
            return objectMapper.readValue(content, valueType);
        } catch (IOException e) {
            log.error("json deserialize failed");
            throw new CommonException(e);
        }
    }
    
    public static TypeFactory getTypeFactory(){
        return objectMapper.getTypeFactory();
    }

    public static ObjectMapper getObjectMapper() {
        return objectMapper;
    }
}
