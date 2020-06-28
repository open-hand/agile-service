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
import org.springframework.util.ObjectUtils;

import java.io.IOException;
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

    static EncryptionService encryptionService = new EncryptionService(new EncryptProperties());

    private static ObjectMapper objectMapper = new ObjectMapper();

    private static String[] IGNORE_VALUES = {"0"};

    /**
     * 解密serachVO
     *
     * @param search SearchVO
     */
    public static void decryptSearchVO(SearchVO search) {
        Optional<Map<String, Object>> adMapOptional = Optional.ofNullable(search).map(SearchVO::getAdvancedSearchArgs);
        if (adMapOptional.isPresent()) {
            decryptAd(search, adMapOptional);
        }

        Optional<Map<String, Object>> searchArgs = Optional.ofNullable(search).map(SearchVO::getOtherArgs);
        if (searchArgs.isPresent()) {
            decryptOa(search, searchArgs);
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
    public static List<Long> decryptList(List<String> crypts, String tableName, String[] ignoreValue) {
        List<Long> cryptsLong = new ArrayList<>();
        List<String> ignoreValueList = new ArrayList<>();
        if (!ArrayUtils.isEmpty(ignoreValue)) {
            ignoreValueList.addAll(Arrays.asList(ignoreValue));
        }
        if (!CollectionUtils.isEmpty(crypts)) {
            for (String crypt : crypts) {
                if (!CollectionUtils.isEmpty(ignoreValueList) && ignoreValueList.contains(crypt)) {
                    cryptsLong.add(Long.valueOf(crypt));
                } else {
                    cryptsLong.add(decrypt(crypt, tableName));
                }
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
     *
     * @param search        SearchVO
     * @param adMapOptional adMapOptional
     */
    @SuppressWarnings("unchecked")
    private static void decryptAd(SearchVO search, Optional<Map<String, Object>> adMapOptional) {
        List<String> temp;
        String tempStr;// versionList
        temp = adMapOptional.map(ad -> (List<String>) (ad.get("versionList"))).orElse(null);
        if (CollectionUtils.isNotEmpty(temp)) {
            search.getAdvancedSearchArgs().put("versionList",
                    temp.stream().map(item -> Long.parseLong(encryptionService.decrypt(item, EncryptionConstant.BLANK_KEY))).collect(Collectors.toList()));
        }
        // statusList
        temp = adMapOptional.map(ad -> (List<String>) (ad.get("statusList"))).orElse(null);
        if (CollectionUtils.isNotEmpty(temp)) {
            search.getAdvancedSearchArgs().put("statusList",
                    temp.stream().map(item -> Long.parseLong(encryptionService.decrypt(item, EncryptionConstant.BLANK_KEY))).collect(Collectors.toList()));
        }
        // components
        temp = adMapOptional.map(ad -> (List<String>) (ad.get("components"))).orElse(null);
        if (CollectionUtils.isNotEmpty(temp)) {
            search.getAdvancedSearchArgs().put("components",
                    temp.stream().map(item -> Long.parseLong(encryptionService.decrypt(item, EncryptionConstant.BLANK_KEY))).collect(Collectors.toList()));
        }
        // sprints
        temp = adMapOptional.map(ad -> (List<String>) (ad.get("sprints"))).orElse(null);
        if (CollectionUtils.isNotEmpty(temp)) {
            search.getAdvancedSearchArgs().put("sprints",
                    temp.stream().map(item -> Long.parseLong(encryptionService.decrypt(item, EncryptionConstant.BLANK_KEY))).collect(Collectors.toList()));
        }
        // statusIdList
        temp = adMapOptional.map(ad -> (List<String>) (ad.get("statusIdList"))).orElse(null);
        if (CollectionUtils.isNotEmpty(temp)) {
            search.getAdvancedSearchArgs().put("statusIdList",
                    temp.stream().map(item -> Long.parseLong(encryptionService.decrypt(item, EncryptionConstant.BLANK_KEY))).collect(Collectors.toList()));
        }
        // prioritys
        temp = adMapOptional.map(ad -> (List<String>) (ad.get("prioritys"))).orElse(null);
        if (CollectionUtils.isNotEmpty(temp)) {
            search.getAdvancedSearchArgs().put("prioritys",
                    temp.stream().map(item -> Long.parseLong(encryptionService.decrypt(item, EncryptionConstant.BLANK_KEY))).collect(Collectors.toList()));
        }

        // issueTypeId
        tempStr = adMapOptional.map(ad -> (String) (ad.get("issueTypeId"))).orElse(null);
        if (StringUtils.isNotBlank(tempStr)) {
            handlerPrimaryKey(tempStr, "issueTypeId", search.getAdvancedSearchArgs());
        }

        // statusId
        tempStr = adMapOptional.map(ad -> (String) (ad.get("statusId"))).orElse(null);
        if (StringUtils.isNotBlank(tempStr)) {
            handlerPrimaryKey(tempStr, "statusId", search.getAdvancedSearchArgs());
        }

        // priorityId
        tempStr = adMapOptional.map(ad -> (String) (ad.get("priorityId"))).orElse(null);
        if (StringUtils.isNotBlank(tempStr)) {
            handlerPrimaryKey(tempStr, "priorityId", search.getAdvancedSearchArgs());
        }
    }

    /**
     * 解密ad
     *
     * @param search        SearchVO
     * @param adMapOptional adMapOptional
     */
    @SuppressWarnings("unchecked")
    private static void decryptOa(SearchVO search, Optional<Map<String, Object>> adMapOptional) {
        List<String> temp;
        String tempStr;// versionList
        // priorityId
        tempStr = adMapOptional.map(ad -> (String) (ad.get("priorityId"))).orElse(null);
        if (StringUtils.isNotBlank(tempStr)) {
            handlerPrimaryKey(tempStr, "priorityId", search.getOtherArgs());
        }

        // component
        tempStr = adMapOptional.map(ad -> (String) (ad.get("component"))).orElse(null);
        if (StringUtils.isNotBlank(tempStr)) {
            handlerPrimaryKey(tempStr, "component", search.getOtherArgs());
        }

        // version
        tempStr = adMapOptional.map(ad -> (String) (ad.get("version"))).orElse(null);
        if (StringUtils.isNotBlank(tempStr)) {
            handlerPrimaryKey(tempStr, "version", search.getOtherArgs());
        }

        // sprint
        tempStr = adMapOptional.map(ad -> (String) (ad.get("sprint"))).orElse(null);
        if (StringUtils.isNotBlank(tempStr)) {
            handlerPrimaryKey(tempStr, "sprint", search.getOtherArgs());
        }

        // issueIds
        tempStr = adMapOptional.map(ad -> (String) (ad.get("issueIds"))).orElse(null);
        if (StringUtils.isNotBlank(tempStr)) {
            handlerPrimaryKey(tempStr, "issueIds", search.getOtherArgs());
        }

        // label
        tempStr = adMapOptional.map(ad -> (String) (ad.get("label"))).orElse(null);
        if (StringUtils.isNotBlank(tempStr)) {
            handlerPrimaryKey(tempStr, "label", search.getOtherArgs());
        }

        // componentIds
        tempStr = adMapOptional.map(ad -> (String) (ad.get("componentIds"))).orElse(null);
        if (StringUtils.isNotBlank(tempStr)) {
            handlerPrimaryKey(tempStr, "componentIds", search.getOtherArgs());
        }
    }

    public static void handlerPrimaryKey(String tempStr, String key, Map<String, Object> map) {
        JsonNode jsonNode = null;
        try {
            jsonNode = objectMapper.readTree(tempStr);
            if (jsonNode.isArray()) {
                List list = objectMapper.readValue(tempStr, List.class);
                map.put(key, decryptList(list, EncryptionConstant.BLANK_KEY,IGNORE_VALUES));
            } else {
                map.put(key, encryptionService.decrypt(tempStr, EncryptionConstant.BLANK_KEY));
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static List<String> encryptList(List<Long> parentIds) {
        List<String> list = new ArrayList<>();
        if(!CollectionUtils.isEmpty(parentIds)){
            parentIds.forEach(v -> list.add(encryptionService.encrypt(v.toString(),EncryptionConstant.BLANK_KEY)));
        }
        return  list;
    }

    public static Map<String,List<String>> encryptMap(Map<Long, List<Long>> parentWithSubs) {
        Map<String,List<String>> map = new HashMap<>();
        if(!parentWithSubs.isEmpty()){
            Iterator<Map.Entry<Long, List<Long>>> iterator = parentWithSubs.entrySet().iterator();
            while (iterator.hasNext()) {
                Map.Entry<Long, List<Long>> next = iterator.next();
                map.put(encryptionService.encrypt(next.getKey().toString(),EncryptionConstant.BLANK_KEY),encryptList(next.getValue()));
            }
        }
        return map;
    }
    public static Map<String, Object> encryptMapKey(Map<Long, ? extends Object> map) {
        Map<String, Object> result = new HashMap<>();
        if (!ObjectUtils.isEmpty(map)) {
            for (Map.Entry<Long, ? extends Object> entry : map.entrySet()) {
                Long key = entry.getKey();
                Object value = entry.getValue();
                String encryptKey = encryptionService.encrypt(key.toString(), EncryptionConstant.BLANK_KEY);
                result.put(encryptKey, value);
            }
        }
        return result;
    }
}
