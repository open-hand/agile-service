package io.choerodon.agile.infra.utils;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import io.choerodon.agile.infra.annotation.Update;
import io.choerodon.core.exception.CommonException;
import org.apache.commons.lang.ArrayUtils;
import org.hzero.starter.keyencrypt.core.Encrypt;
import org.springframework.stereotype.Component;
import org.springframework.util.ObjectUtils;

import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;

/**
 * @author dinghuang123@gmail.com
 */
@Component
public class VerifyUpdateUtil {

    /**
     * 根据前端数据进行部分更新
     *
     * @param updateMap    updateMap
     * @param objectUpdate objectUpdate
     * @return String
     */
    public List<String> verifyUpdateData(JSONObject updateMap, Object objectUpdate) {
        List<String> fieldList = new ArrayList<>();
        Class objectClass = objectUpdate.getClass();
        updateMap.forEach((String k, Object v) -> {
            try {
                Field field = objectClass.getDeclaredField(k);
                field.setAccessible(true);
                Boolean flag = true;
                Update update = field.getAnnotation(Update.class);
                if (update != null && update.temp()) {
                    flag = false;
                }
                flag = handleFieldType(field, objectUpdate, v, flag);
                if (flag && update == null) {
                    fieldList.add(k);
                }
                if (update != null && !Objects.equals(update.name(), "")) {
                    fieldList.add(update.name());
                }
            } catch (Exception e) {
                throw new CommonException("error.verifyUpdateData.noField", e);
            }
        });
        return fieldList;
    }

    private Boolean handleFieldType(Field field, Object objectUpdate, Object v, Boolean flag) throws
            IllegalAccessException, ParseException, ClassNotFoundException, InstantiationException {
        if (field.getType() == String.class) {
            field.set(objectUpdate, v);
        } else if (field.getType() == Long.class) {
            handlerDecryptionLong(field,objectUpdate,v);
        } else if (field.getType() == Date.class) {
            SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
            field.set(objectUpdate, v != null ? sdf.parse(v.toString()) : null);
        } else if (field.getType() == Integer.class) {
            field.set(objectUpdate, v == null ? null : Integer.valueOf(v.toString()));
        } else if (field.getType() == BigDecimal.class) {
            field.set(objectUpdate, v == null ? null : new BigDecimal(v.toString()));
        } else if (field.getType() == List.class) {
            //对象包含子对象是list的值设置
            handlerListObject(field, objectUpdate, v);
            flag = false;
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
                field.set(objectUpdate,EncryptionUtils.decrypt(v.toString(), fieldAnnotation.value()));
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
        String className = field.getGenericType().getTypeName().substring(15, field.getGenericType().getTypeName().length() - 1);
        Class<?> forName = Class.forName(className);
        if (forName.isPrimitive() || EncryptionUtils.isWrapClass(forName) || forName.newInstance() instanceof String) {
            if (forName == Long.class) {
                Encrypt declaredAnnotation = field.getDeclaredAnnotation(Encrypt.class);
                if (!ObjectUtils.isEmpty(declaredAnnotation)) {
                    String[] ignoreValue = declaredAnnotation.ignoreValue();
                    field.set(objectUpdate, EncryptionUtils.decryptList(JSON.parseArray(v.toString(), String.class), declaredAnnotation.value(),ignoreValue));
                } else {
                    String json = JSON.toJSONString(v);
                    field.set(objectUpdate, JSON.parseArray(json, forName));
                }
            } else {
                String json = JSON.toJSONString(v);
                field.set(objectUpdate, JSON.parseArray(json, forName));
            }
        } else {
            JSONArray jsonArray = JSONObject.parseArray(JSON.toJSONString(v));
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
                        e.printStackTrace();
                    }
                });
                list.add(obj);
            }
            field.set(objectUpdate, list);
        }
    }

}
