package io.choerodon.agile.infra.utils;

import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import org.apache.commons.lang.ArrayUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.ObjectUtils;

import io.choerodon.agile.app.service.AgilePluginService;
import io.choerodon.agile.infra.annotation.Update;
import io.choerodon.core.exception.CommonException;

import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author dinghuang123@gmail.com
 */
@Component
public class VerifyUpdateUtil {

    private static final Logger LOGGER = LoggerFactory.getLogger(VerifyUpdateUtil.class);

    @Autowired(required = false)
    private AgilePluginService agilePluginService;

    /**
     * 根据前端数据进行部分更新
     *
     * @param updateMap    updateMap
     * @param objectUpdate objectUpdate
     * @return String
     */
    public List<String> verifyUpdateData(JSONObject updateMap, Object objectUpdate) {
        List<String> fieldList = new ArrayList<>();
        Class<?> objectClass = objectUpdate.getClass();
        for (Map.Entry<String, Object> entry : updateMap.entrySet()) {
            String fieldCode = entry.getKey();
            Object fieldValue = entry.getValue();
            try {
                Field field = objectClass.getDeclaredField(fieldCode);
                field.setAccessible(true);
                Boolean flag = true;
                Update update = field.getAnnotation(Update.class);
                if (update != null && update.temp()) {
                    flag = false;
                }
                flag = handleFieldType(field, objectUpdate, fieldValue, flag);
                if (flag && update == null) {
                    fieldList.add(fieldCode);
                }
                if (update != null && !Objects.equals(update.name(), "")) {
                    fieldList.add(update.name());
                }
            } catch (Exception e) {
                throw new CommonException("error.verifyUpdateData.noField", e);
            }
        }
        if (agilePluginService != null) {
            agilePluginService.verifyUpdateData(updateMap, fieldList);
        }
        return fieldList;
    }

    private Boolean handleFieldType(Field field, Object objectUpdate, Object v, Boolean flag) throws
            IllegalAccessException, ParseException, ClassNotFoundException, InstantiationException {
        Class<?> type = field.getType();
        if (type == String.class) {
            field.set(objectUpdate, v);
        } else if (type == Long.class) {
            handlerDecryptionLong(field,objectUpdate,v);
        } else if (type == Date.class) {
            SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
            field.set(objectUpdate, v != null ? sdf.parse(v.toString()) : null);
        } else if (type == Integer.class) {
            field.set(objectUpdate, v == null ? null : Integer.valueOf(v.toString()));
        } else if (type == BigDecimal.class) {
            field.set(objectUpdate, v == null ? null : new BigDecimal(v.toString()));
        } else if (type == List.class) {
            //对象包含子对象是list的值设置
            handlerListObject(field, objectUpdate, v);
            flag = false;
        }
        else if (!ObjectUtils.isEmpty(type.getClassLoader())) {
            // 是一个自定义的类
            Object obj = type.newInstance();
            EncryptionUtils.handlerObject(JSON.toJSONString(v),obj, type);
            field.set(objectUpdate, obj);
            flag = false;
        }
        else if (type == Boolean.class) {
            field.set(objectUpdate,v);
        }
        return flag;
    }

    private void handlerDecryptionLong(Field field, Object objectUpdate, Object v) throws IllegalAccessException {
        Encrypt fieldAnnotation = field.getDeclaredAnnotation(Encrypt.class);
        if (ObjectUtils.isEmpty(fieldAnnotation)) {
            field.set(objectUpdate, v == null ? null : Long.valueOf(v.toString()));
        } else {
            String[] ignoreValue = fieldAnnotation.ignoreValue();
            if(ArrayUtils.isEmpty(ignoreValue)){
                field.set(objectUpdate, v == null ? null : EncryptionUtils.decrypt(v.toString(), fieldAnnotation.value()));
            }
            else {
                if(Arrays.asList(ignoreValue).contains(v)){
                    field.set(objectUpdate, v == null ? null : Long.valueOf(v.toString()));
                } else {
                    field.set(objectUpdate,EncryptionUtils.decrypt(v.toString(), fieldAnnotation.value()));
                }
            }
        }
    }

    private void  handlerListObject(Field field, Object objectUpdate, Object v) throws
            IllegalAccessException, ClassNotFoundException, InstantiationException {
        if (v == null) {
            return;
        }
        String className = field.getGenericType().getTypeName().substring(15, field.getGenericType().getTypeName().length() - 1);
        Class<?> forName = Class.forName(className);
        if (forName.isPrimitive() || EncryptionUtils.isWrapClass(forName) || forName.newInstance() instanceof String) {
            if (forName == Long.class) {
                Encrypt declaredAnnotation = field.getDeclaredAnnotation(Encrypt.class);
                if (!ObjectUtils.isEmpty(declaredAnnotation)) {
                    String[] ignoreValue = declaredAnnotation.ignoreValue();
                    field.set(objectUpdate, EncryptionUtils.decryptList(JSON.parseArray(JSON.toJSONString(v), String.class), declaredAnnotation.value(),ignoreValue));
                } else {
                    String json = JSON.toJSONString(v);
                    field.set(objectUpdate, JSON.parseArray(json, forName));
                }
            } else {
                String json = JSON.toJSONString(v);
                field.set(objectUpdate, JSON.parseArray(json, forName));
            }
        } else {
            JSONArray jsonArray = JSON.parseArray(JSON.toJSONString(v));
            List list = new ArrayList();
            for (Object objValue : jsonArray) {
                JSONObject jsonObject = JSON.parseObject(objValue.toString());
                Object obj = forName.newInstance();
                jsonObject.forEach((String k, Object value) -> {
                    try {
                        Field field1 = forName.getDeclaredField(k);
                        field1.setAccessible(true);
                        handleFieldType(field1, obj, value, false);
                    } catch (Exception e) {
                        LOGGER.error("reflect error");
                        LOGGER.error(e.getMessage(), e);
                    }
                });
                list.add(obj);
            }
            field.set(objectUpdate, list);
        }
    }

}
