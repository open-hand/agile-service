package io.choerodon.agile.infra.utils;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.choerodon.agile.api.vo.SearchVO;
import io.choerodon.agile.infra.constants.EncryptionConstant;
import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.hzero.starter.keyencrypt.core.Encrypt;
import org.hzero.starter.keyencrypt.core.EncryptProperties;
import org.hzero.starter.keyencrypt.core.EncryptionService;
import org.apache.commons.collections4.CollectionUtils;

import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @author zhaotianxin
 * @date 2020-06-16 20:24
 */
public class EncryptionUtils {

    private static EncryptionService encryptionService = new EncryptionService(new EncryptProperties());

    private static ObjectMapper objectMapper = new ObjectMapper();

    /**
     * 解密serachVO
     * @param search SearchVO
     */
    public static void decryptSearchVO(SearchVO search){
        Optional<Map<String, Object>> adMapOptional = Optional.ofNullable(search).map(SearchVO::getAdvancedSearchArgs);
        if (adMapOptional.isPresent()){
            decryptAd(search, adMapOptional);
        }
    }

    /**
     * 对单个主键进行解密
     *
     * @param crypt
     * @param tableName
     * @return
     */
    public static Long decrypt(String crypt, String tableName) {
        if (StringUtils.isNumeric(crypt)) {
            return Long.parseLong(crypt);
        } else {
            return Long.parseLong(encryptionService.decrypt(crypt, tableName));
        }
    }

    /**
     * 解密List<String>形式的主键
     *
     * @param crypts
     * @param tableName
     * @return
     */
    public static List<Long> decryptList(List<String> crypts, String tableName) {
        List<Long> cryptsLong = new ArrayList<>();
        if (!CollectionUtils.isEmpty(crypts)) {
            for (String crypt : crypts) {
                cryptsLong.add(decrypt(crypt, tableName));
            }
        }
        return cryptsLong;
    }

    /***
     * 处理List 对象
     * @param object
     * @param clazz
     * @return
     */
    public static List jsonToList(Object object, Class clazz) {
        List list = new ArrayList();
        try {
            JsonNode jsonNode = objectMapper.readTree(object.toString());
            if (jsonNode.isArray()) {
                Iterator<JsonNode> elements = jsonNode.elements();
                while (elements.hasNext()) {
                    JsonNode next = elements.next();
                    Object obj;
                    if (next.isObject()) {
                        obj = clazz.newInstance();
                        handlerObject(next.toString(), obj, clazz);
                    } else {
                        obj = objectMapper.readValue(next.toString(), clazz);
                    }
                    list.add(obj);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }

        return list;
    }

    /**
     * 处理Object 对象
     *
     * @param object
     * @param object
     * @param clazz
     */
    public static void handlerObject(String jsonString, Object object, Class clazz) {
        try {
            JsonNode jsonNode = objectMapper.readTree(jsonString);
            Iterator<String> fieldNames = jsonNode.fieldNames();
            while (fieldNames.hasNext()) {
                String fieldName = fieldNames.next();
                JsonNode valueNode = jsonNode.get(fieldName);
                Field field = null;
                try {
                    field = clazz.getDeclaredField(fieldName);
                    field.setAccessible(true);
                    if (field.getType() == String.class) {
                        field.set(object, valueNode.toString());
                    } else if (field.getType() == Long.class) {
                        Encrypt encrypt = field.getAnnotation(Encrypt.class);
                        String[] ignoreValues = encrypt.ignoreValue();
                        if (encrypt != null && !StringUtils.isNumeric(valueNode.toString())) {
                            field.set(object, decrypt(valueNode.toString(), encrypt.value()));
                        } else if (encrypt != null && !ArrayUtils.isEmpty(ignoreValues) && Arrays.asList(ignoreValues).contains(valueNode.toString())) {
                            field.set(object, Long.valueOf(valueNode.toString()));
                        } else {
                            field.set(object, valueNode == null ? null : Long.valueOf(valueNode.toString()));
                        }
                    } else if (field.getType() == Date.class) {
                        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
                        field.set(object, valueNode != null ? sdf.parse(valueNode.toString()) : null);
                    } else if (field.getType() == Integer.class) {
                        field.set(object, valueNode == null ? null : Integer.valueOf(valueNode.toString()));
                    } else if (field.getType() == BigDecimal.class) {
                        field.set(object, valueNode == null ? null : new BigDecimal(valueNode.toString()));
                    } else if (field.getType() == List.class) {
                        String className = field.getGenericType().getTypeName().substring(15, field.getGenericType().getTypeName().length() - 1);
                        Class<?> aClass = Class.forName(className);
                        List list = jsonToList(valueNode, aClass);
                        field.set(object, list);
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static boolean isWrapClass(Class clz) {
        try {
            return ((Class) clz.getField("TYPE").get(null)).isPrimitive();
        } catch (Exception e) {
            return false;
        }
    }


    /**
     * 解密ad
     * @param search SearchVO
     * @param adMapOptional adMapOptional
     */
    @SuppressWarnings("unchecked")
    private static void decryptAd(SearchVO search, Optional<Map<String, Object>> adMapOptional) {
        List<String> temp;
        String tempStr;// versionList
        temp = adMapOptional.map(ad -> (List<String>)(ad.get("versionList"))).orElse(null);
        if (CollectionUtils.isNotEmpty(temp)){
            search.getAdvancedSearchArgs().put("versionList",
                    temp.stream().map(item -> Long.parseLong(encryptionService.decrypt(item, EncryptionConstant.BLANK_KEY))).collect(Collectors.toList()));
        }
        // statusList
        temp = adMapOptional.map(ad -> (List<String>)(ad.get("statusList"))).orElse(null);
        if (CollectionUtils.isNotEmpty(temp)){
            search.getAdvancedSearchArgs().put("statusList",
                    temp.stream().map(item -> Long.parseLong(encryptionService.decrypt(item, EncryptionConstant.BLANK_KEY))).collect(Collectors.toList()));
        }
        // components
        temp = adMapOptional.map(ad -> (List<String>)(ad.get("components"))).orElse(null);
        if (CollectionUtils.isNotEmpty(temp)){
            search.getAdvancedSearchArgs().put("components",
                    temp.stream().map(item -> Long.parseLong(encryptionService.decrypt(item, EncryptionConstant.BLANK_KEY))).collect(Collectors.toList()));
        }
        // sprints
        temp = adMapOptional.map(ad -> (List<String>)(ad.get("sprints"))).orElse(null);
        if (CollectionUtils.isNotEmpty(temp)){
            search.getAdvancedSearchArgs().put("sprints",
                    temp.stream().map(item -> Long.parseLong(encryptionService.decrypt(item, EncryptionConstant.BLANK_KEY))).collect(Collectors.toList()));
        }
        // statusIdList
        temp = adMapOptional.map(ad -> (List<String>)(ad.get("statusIdList"))).orElse(null);
        if (CollectionUtils.isNotEmpty(temp)){
            search.getAdvancedSearchArgs().put("statusIdList",
                    temp.stream().map(item -> Long.parseLong(encryptionService.decrypt(item, EncryptionConstant.BLANK_KEY))).collect(Collectors.toList()));
        }
        // prioritys
        temp = adMapOptional.map(ad -> (List<String>)(ad.get("prioritys"))).orElse(null);
        if (CollectionUtils.isNotEmpty(temp)){
            search.getAdvancedSearchArgs().put("prioritys",
                    temp.stream().map(item -> Long.parseLong(encryptionService.decrypt(item, EncryptionConstant.BLANK_KEY))).collect(Collectors.toList()));
        }
        // assigneeId
        tempStr = adMapOptional.map(ad -> (String)(ad.get("assigneeId"))).orElse(null);
        if (StringUtils.isNotBlank(tempStr)){
            search.getAdvancedSearchArgs().put("assigneeId", encryptionService.decrypt(tempStr, EncryptionConstant.BLANK_KEY));
        }
    }
}
