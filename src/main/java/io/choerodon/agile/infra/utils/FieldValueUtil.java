package io.choerodon.agile.infra.utils;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import io.choerodon.agile.api.vo.FieldDataLogCreateVO;
import io.choerodon.agile.api.vo.ObjectSchemeFieldDetailVO;
import io.choerodon.agile.api.vo.PageFieldViewVO;
import io.choerodon.agile.app.service.FieldDataLogService;
import io.choerodon.agile.infra.dto.FieldOptionDTO;
import io.choerodon.agile.infra.dto.FieldValueDTO;
import io.choerodon.agile.infra.dto.PageFieldDTO;
import io.choerodon.agile.infra.dto.UserDTO;
import io.choerodon.agile.infra.enums.FieldType;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.mapper.FieldOptionMapper;
import io.choerodon.core.exception.CommonException;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

/**
 * @author shinan.chen
 * @since 2019/6/11
 */
public class FieldValueUtil {

    private FieldValueUtil() {}

    private static final String DATETIME_FORMAT = "yyyy-MM-dd HH:mm:ss";
    /**
     * 匹配 Thu Apr 30 2020 00:00:00 GMT+0800 格式的时间
     */
    private static final String ENGLISH_STRING_DATE_FORMAT = "EEE MMM dd yyyy HH:mm:ss 'GMT'Z";
    private static final String DATE_FORMAT = "yyyy-MM-dd";
    private static final String TIME_FORMAT = "HH:mm:ss";
    private static final String DATE_VALUE = "date_value";

    /**
     * 获取人员信息
     *
     * @param userIds
     * @return
     */
    public static Map<Long, UserDTO> handleUserMap(List<Long> userIds) {
        Map<Long, UserDTO> map = new HashMap<>(userIds.size());
        BaseFeignClient baseFeignClient = SpringBeanUtil.getBean(BaseFeignClient.class);
        if (!userIds.isEmpty()) {
            map = baseFeignClient.listUsersByIds(userIds.toArray(new Long[userIds.size()]), false).getBody().stream().collect(Collectors.toMap(UserDTO::getId, x -> x));
        }
        return map;
    }

    /**
     * 处理FieldValueDTO为value
     *
     * @param view
     * @param fieldType
     * @param values
     */
    public static void handleDTO2Value(PageFieldViewVO view, String fieldType, List<FieldValueDTO> values, Map<Long, UserDTO> userMap, Boolean isJustStr) {
        Object valueStr = null;
        Object value = null;
        if (values != null && !values.isEmpty()) {
            Long[] longValues = new Long[values.size()];
            switch (fieldType) {
                case FieldType.CHECKBOX:
                case FieldType.MULTIPLE:
                    values.stream().map(FieldValueDTO::getOptionId).collect(Collectors.toList()).toArray(longValues);
                    value = EncryptionUtils.encryptList(Arrays.asList(longValues));
                    valueStr = values.stream().map(FieldValueDTO::getOptionValue).collect(Collectors.joining(", "));
                    break;
                case FieldType.RADIO:
                case FieldType.SINGLE:
                    //单选款/选择器（单选）获取为Long
                    value = EncryptionUtils.encrypt(values.get(0).getOptionId());
                    valueStr = values.get(0).getOptionValue();
                    break;
                case FieldType.DATETIME:
                    value = values.get(0).getDateValue();
                    DateFormat dff = new SimpleDateFormat(DATETIME_FORMAT);
                    if (value != null) {
                        valueStr = dff.format(value);
                    }
                    break;
                case FieldType.DATE:
                    value = values.get(0).getDateValue();
                    DateFormat dfff = new SimpleDateFormat(DATE_FORMAT);
                    if (value != null) {
                        valueStr = dfff.format(value);
                    }
                    break;
                case FieldType.TIME:
                    value = values.get(0).getDateValue();
                    DateFormat df = new SimpleDateFormat(TIME_FORMAT);
                    if (value != null) {
                        valueStr = df.format(value);
                    }
                    break;
                case FieldType.INPUT:
                    value = values.get(0).getStringValue();
                    valueStr = value.toString();
                    break;
                case FieldType.NUMBER:
                    value = values.get(0).getNumberValue();
                    //是否包括小数
                    if (view.getExtraConfig() != null && view.getExtraConfig()) {
                        valueStr = value.toString();
                    } else {
                        valueStr = value.toString().split("\\.")[0];
                        value = valueStr;
                    }
                    break;
                case FieldType.TEXT:
                    value = values.get(0).getTextValue();
                    valueStr = value.toString();
                    break;
                case FieldType.MEMBER:
                    //人员获取为Long
                    value = values.get(0).getOptionId();
                    //是否仅需要字符串，用于导出
                    if (isJustStr) {
                        UserDTO userVO = userMap.get(value);
                        if (userVO != null) {
                            valueStr = userVO.getRealName() + "（"+ userVO.getEmail() + "）";
                        }
                    } else {
                        valueStr = userMap.getOrDefault(value, new UserDTO());
                    }
                    // 用户id加密
                    value = EncryptionUtils.encrypt(values.get(0).getOptionId());
                    break;
                case FieldType.MULTI_MEMBER:
                    List<Long> optionIds = values.stream().map(FieldValueDTO::getOptionId).collect(Collectors.toList());
                    if(!CollectionUtils.isEmpty(optionIds)){
                        //是否仅需要字符串，用于导出
                        if (isJustStr) {
                            List<String> userStr = new ArrayList<>();
                            optionIds.forEach(v -> {
                                UserDTO userVO = userMap.get(v);
                                if (userVO != null) {
                                    userStr.add(userVO.getRealName() + "（"+ userVO.getEmail() + "）");
                                }
                            });
                            valueStr =  userStr.stream().collect(Collectors.joining(","));
                        } else {
                           List<UserDTO> userDTOList = new ArrayList<>();
                           optionIds.forEach(v -> {
                               UserDTO userDTO = userMap.get(v);
                               if (!ObjectUtils.isEmpty(userDTO)) {
                                   userDTOList.add(userDTO);
                               }
                           });
                           valueStr = userDTOList;
                        }
                        // 用户id加密
                        value = EncryptionUtils.encryptList(optionIds);
                    }
                    break;
                default:
                    break;
            }
        }
        view.setValueStr(valueStr);
        view.setValue(value);
    }

    /**
     * 处理默认值
     *
     * @param pageFieldViews
     */
    public static void handleDefaultValue(List<PageFieldViewVO> pageFieldViews) {
        List<Long> userIds = getUserIds(pageFieldViews);
        Map<Long, UserDTO> userMap = handleUserMap(userIds);
        for (PageFieldViewVO view : pageFieldViews) {
            switch (view.getFieldType()) {
                case FieldType.CHECKBOX:
                case FieldType.MULTIPLE:
                    handleDefaultValueIds(view);
                    break;
                case FieldType.RADIO:
                case FieldType.SINGLE:
                    if (view.getDefaultValue() != null && !"".equals(view.getDefaultValue())) {
                        view.setDefaultValue(EncryptionUtils.encrypt(Long.valueOf(String.valueOf(view.getDefaultValue()))));
                    }
                    break;
                case FieldType.DATETIME:
                case FieldType.DATE:
                case FieldType.TIME:
                    //如果勾选了默认当前
                    if (view.getExtraConfig() != null && view.getExtraConfig()) {
                        view.setDefaultValue(new Date());
                    }
                    break;
                case FieldType.NUMBER:
                    //如果勾选了是否小数
                    if (view.getExtraConfig() != null && !view.getExtraConfig() && !ObjectUtils.isEmpty(view.getDefaultValue())) {
                        view.setDefaultValue(view.getDefaultValue().toString().split("\\.")[0]);
                    }
                    break;
                case FieldType.MEMBER:
                    if (view.getDefaultValue() != null && !"".equals(view.getDefaultValue())) {
                        Long defaultValue = Long.valueOf(String.valueOf(view.getDefaultValue()));
                        view.setDefaultValue(EncryptionUtils.encrypt(defaultValue));
                        view.setDefaultValueObj(userMap.getOrDefault(defaultValue, new UserDTO()));
                    }
                    break;
                case FieldType.MULTI_MEMBER:
                     handlerMultiMember(view, userMap);
                    break;
                case FieldType.INPUT:
                case FieldType.TEXT:
                    break;
                default:
                    break;
            }
        }
    }

    private static void handlerMultiMember(PageFieldViewVO view, Map<Long, UserDTO> userMap) {
        Object defaultValue = view.getDefaultValue();
        if (defaultValue != null && !"".equals(defaultValue)) {
            String[] defaultIdStrs = String.valueOf(defaultValue).split(",");
            if (defaultIdStrs != null) {
                String[] defaultIds = new String[defaultIdStrs.length];
                List<Object> userDTOS = new ArrayList<>();
                for (int i = 0; i < defaultIdStrs.length; i++) {
                    defaultIds[i] = EncryptionUtils.encrypt(Long.valueOf(defaultIdStrs[i]));
                    UserDTO userDTO = userMap.get(Long.valueOf(defaultIdStrs[i]));
                    if (!ObjectUtils.isEmpty(userDTO)) {
                        userDTOS.add(userDTO);
                    }
                }
                view.setDefaultValue(defaultIds);
                view.setDefaultValueObjs(userDTOS);
            }
        }
    }

    private static List<Long> getUserIds(List<PageFieldViewVO> pageFieldViews) {
        List<Long> userIds = new ArrayList<>();
        pageFieldViews.stream().filter(x -> (x.getFieldType().equals(FieldType.MEMBER) || x.getFieldType().equals(FieldType.MULTI_MEMBER)) && x.getDefaultValue() != null && !"".equals(String.valueOf(x.getDefaultValue()).trim()))
                .forEach(v -> {
                    if (Objects.equals(FieldType.MEMBER, v.getFieldType())) {
                        userIds.add(Long.valueOf(v.getDefaultValue().toString()));
                    } else {
                        String[] split = v.getDefaultValue().toString().split(",");
                        List<Long> ids = Stream.of(split).map(Long::valueOf).collect(Collectors.toList());
                        userIds.addAll(ids);
                    }
                });
        return userIds;
    }

    /**
     * 处理默认值多选id
     *
     * @param view
     */
    public static void handleDefaultValueIds(PageFieldViewVO view) {
        Object defaultValue = view.getDefaultValue();
        if (defaultValue != null && !"".equals(defaultValue)) {
            String[] defaultIdStrs = String.valueOf(defaultValue).split(",");
            if (defaultIdStrs != null) {
                String[] defaultIds = new String[defaultIdStrs.length];
                for (int i = 0; i < defaultIdStrs.length; i++) {
                    defaultIds[i] = EncryptionUtils.encrypt(Long.valueOf(defaultIdStrs[i]));
                }
                view.setDefaultValue(defaultIds);
            }
        }
    }

    /**
     * 处理默认值
     *
     * @param fieldDetail
     */
    public static void handleDefaultValue(ObjectSchemeFieldDetailVO fieldDetail) {
        switch (fieldDetail.getFieldType()) {
            case FieldType.CHECKBOX:
            case FieldType.MULTIPLE:
            case FieldType.RADIO:
            case FieldType.SINGLE:
            case FieldType.DATETIME:
            case FieldType.DATE:
            case FieldType.TIME:
            case FieldType.NUMBER:
            case FieldType.INPUT:
            case FieldType.TEXT:
                break;
            case FieldType.MEMBER:
                BaseFeignClient baseFeignClient = SpringBeanUtil.getBean(BaseFeignClient.class);
                if (fieldDetail.getDefaultValue() != null && !"".equals(fieldDetail.getDefaultValue())) {
                    Long defaultValue = Long.valueOf(String.valueOf(fieldDetail.getDefaultValue()));
                    fieldDetail.setDefaultValue(EncryptionUtils.encrypt(defaultValue));
                    List<UserDTO> list = baseFeignClient.listUsersByIds(Arrays.asList(defaultValue).toArray(new Long[1]), false).getBody();
                    if (!list.isEmpty()) {
                        fieldDetail.setDefaultValueObj(list.get(0));
                    }
                }
                break;
            case FieldType.MULTI_MEMBER:
                BaseFeignClient baseFeignClient1 = SpringBeanUtil.getBean(BaseFeignClient.class);
                if (fieldDetail.getDefaultValue() != null && !"".equals(fieldDetail.getDefaultValue())) {
                    String[] split = fieldDetail.getDefaultValue().split(",");
                    List<UserDTO> defaultValueObjects = new ArrayList<>();
                    List<Long> defaultValues = new ArrayList<>();
                    for (String userId : split) {
                        Long defaultValue = Long.valueOf(userId);
                        defaultValues.add(defaultValue);
                        List<UserDTO> list = baseFeignClient1.listUsersByIds(Arrays.asList(defaultValue).toArray(new Long[1]), false).getBody();
                        if (!list.isEmpty()) {
                            defaultValueObjects.addAll(list);
                        }
                    }
                    fieldDetail.setDefaultValue(EncryptionUtils.encryptList(defaultValues).stream().collect(Collectors.joining(",")));
                    fieldDetail.setDefaultValueObj(defaultValueObjects);
                }
                break;
            default:
                break;
        }
    }

    /**
     * 处理valueStr为FieldValue
     *
     * @param fieldValues
     * @param create
     */
    public static void handleDefaultValue2DTO(List<FieldValueDTO> fieldValues, PageFieldDTO create) {
        String defaultValue = create.getDefaultValue();
        String fieldType = create.getFieldType();
        FieldValueDTO fieldValue = new FieldValueDTO();
        //处理默认当前时间
        if (fieldType.equals(FieldType.DATETIME) || fieldType.equals(FieldType.TIME)) {
            if (create.getExtraConfig() != null && create.getExtraConfig()) {
                DateFormat df = new SimpleDateFormat(DATETIME_FORMAT);
                defaultValue = df.format(new Date());
            }
        }
        if (defaultValue != null && !defaultValue.equals("")) {
            try {
                switch (fieldType) {
                    case FieldType.CHECKBOX:
                    case FieldType.MULTI_MEMBER:
                    case FieldType.MULTIPLE:
                        String[] optionIds = defaultValue.split(",");
                        for (String optionId : optionIds) {
                            FieldValueDTO oValue = new FieldValueDTO();
                            oValue.setOptionId(Long.parseLong(optionId));
                            fieldValues.add(oValue);
                        }
                        break;
                    case FieldType.RADIO:
                    case FieldType.SINGLE:
                    case FieldType.MEMBER:
                        Long optionId = Long.parseLong(defaultValue);
                        fieldValue.setOptionId(optionId);
                        fieldValues.add(fieldValue);
                        break;
                    case FieldType.DATETIME:
                    case FieldType.DATE:
                    case FieldType.TIME:
                        convertDate(fieldValues, defaultValue, fieldValue);
                        break;
                    case FieldType.INPUT:
                        fieldValue.setStringValue(defaultValue);
                        fieldValues.add(fieldValue);
                        break;
                    case FieldType.NUMBER:
                        fieldValue.setNumberValue(defaultValue);
                        fieldValues.add(fieldValue);
                        break;
                    case FieldType.TEXT:
                        fieldValue.setTextValue(defaultValue);
                        fieldValues.add(fieldValue);
                        break;
                    default:
                        break;
                }
            } catch (Exception e) {
                throw new CommonException("error.defaultValue.parse", e);
            }
        }
    }

    private static void convertDate(List<FieldValueDTO> fieldValues, String defaultValue, FieldValueDTO fieldValue) throws ParseException {
        Date dateValue;
        //兼容Thu Apr 30 2020 00:00:00 GMT+0800和2020-02-13 15:51:22格式的日期
        try {
            DateFormat df = new SimpleDateFormat(ENGLISH_STRING_DATE_FORMAT, Locale.ENGLISH);
            dateValue = df.parse(defaultValue);
        } catch (ParseException e) {
            DateFormat df = new SimpleDateFormat(DATETIME_FORMAT);
            //yyyy-MM-dd HH:mm:ss格式转换失败，直接抛出异常
            dateValue = df.parse(defaultValue);
        }
        fieldValue.setDateValue(dateValue);
        fieldValues.add(fieldValue);
    }

    /**
     * 处理value为FieldValue
     *
     * @param fieldValues
     * @param fieldType
     * @param value
     */
    public static void handleValue2DTO(List<FieldValueDTO> fieldValues, String fieldType, Object value) {
        FieldValueDTO fieldValue = new FieldValueDTO();
        if (value != null) {
            try {
                switch (fieldType) {
                    case FieldType.CHECKBOX:
                    case FieldType.MULTI_MEMBER:
                    case FieldType.MULTIPLE:
                        List<String> optionIds = (List<String>) value;
                        for (String optionId : optionIds) {
                            FieldValueDTO oValue = new FieldValueDTO();
                            oValue.setOptionId(EncryptionUtils.decrypt(optionId, EncryptionUtils.BLANK_KEY));
                            fieldValues.add(oValue);
                        }
                        break;
                    case FieldType.RADIO:
                    case FieldType.SINGLE:
                    case FieldType.MEMBER:
                        //人员/单选款/选择器（单选）处理为Long
                        Long optionId = EncryptionUtils.decrypt(value.toString(), EncryptionUtils.BLANK_KEY);
                        fieldValue.setOptionId(optionId);
                        fieldValues.add(fieldValue);
                        break;
                    case FieldType.DATETIME:
                    case FieldType.DATE:
                    case FieldType.TIME:
                        convertDate(fieldValues, value.toString(), fieldValue);
                        break;
                    case FieldType.INPUT:
                        String stringValue = (String) value;
                        fieldValue.setStringValue(stringValue);
                        fieldValues.add(fieldValue);
                        break;
                    case FieldType.NUMBER:
                        String numberValue = value.toString();
                        fieldValue.setNumberValue(numberValue);
                        fieldValues.add(fieldValue);
                        break;
                    case FieldType.TEXT:
                        String textValue = (String) value;
                        fieldValue.setTextValue(textValue);
                        fieldValues.add(fieldValue);
                        break;
                    default:
                        break;
                }
            } catch (Exception e) {
                throw new CommonException("error.date.parse", e);
            }
        }
    }

    /**
     * 处理自定义字段日志
     *
     * @param organizationId
     * @param projectId
     * @param instanceId
     * @param fieldId
     * @param fieldType
     * @param schemeCode
     * @param oldFieldValues
     * @param newFieldValues
     */
    public static void handleDataLog(Long organizationId, Long projectId, Long instanceId, Long fieldId, String fieldType, String schemeCode, List<FieldValueDTO> oldFieldValues, List<FieldValueDTO> newFieldValues) {
        handleAgileDataLog(organizationId, projectId, instanceId, fieldId, fieldType, oldFieldValues, newFieldValues, schemeCode);
    }

    /**
     * 处理敏捷问题自定义字段日志
     *
     * @param organizationId
     * @param projectId
     * @param instanceId
     * @param fieldId
     * @param fieldType
     * @param oldFieldValues
     * @param newFieldValues
     */
    private static void handleAgileDataLog(Long organizationId,
                                           Long projectId,
                                           Long instanceId,
                                           Long fieldId,
                                           String fieldType,
                                           List<FieldValueDTO> oldFieldValues,
                                           List<FieldValueDTO> newFieldValues,
                                           String schemeCode) {
        FieldOptionMapper fieldOptionMapper = SpringBeanUtil.getBean(FieldOptionMapper.class);
        FieldDataLogService fieldDataLogService = SpringBeanUtil.getBean(FieldDataLogService.class);
        FieldDataLogCreateVO create = new FieldDataLogCreateVO();
        create.setFieldId(fieldId);
        create.setInstanceId(instanceId);
        try {
            switch (fieldType) {
                case FieldType.CHECKBOX:
                case FieldType.MULTIPLE:
                    if (!oldFieldValues.isEmpty()) {
                        create.setOldValue(oldFieldValues.stream().map(x -> String.valueOf(x.getOptionId())).collect(Collectors.joining(",")));
                        create.setOldString(oldFieldValues.stream().map(FieldValueDTO::getOptionValue).collect(Collectors.joining(",")));
                    }
                    if (!newFieldValues.isEmpty()) {
                        List<Long> newOptionIds = newFieldValues.stream().map(FieldValueDTO::getOptionId).collect(Collectors.toList());
                        create.setNewValue(newOptionIds.stream().map(String::valueOf).collect(Collectors.joining(",")));
                        create.setNewString(fieldOptionMapper.selectByOptionIds(organizationId, newOptionIds).stream().map(FieldOptionDTO::getValue).collect(Collectors.joining(",")));
                    }
                    break;
                case FieldType.RADIO:
                case FieldType.SINGLE:
                    if (!oldFieldValues.isEmpty()) {
                        create.setOldValue(String.valueOf(oldFieldValues.get(0).getOptionId()));
                        create.setOldString(String.valueOf(oldFieldValues.get(0).getOptionValue()));
                    }
                    if (!newFieldValues.isEmpty()) {
                        List<Long> newOptionIds = Arrays.asList(newFieldValues.get(0).getOptionId());
                        List<FieldOptionDTO> fieldOptions = fieldOptionMapper.selectByOptionIds(organizationId, newOptionIds);
                        create.setNewValue(String.valueOf(newFieldValues.get(0).getOptionId()));
                        if (!fieldOptions.isEmpty()) {
                            create.setNewString(fieldOptions.get(0).getValue());
                        }
                    }
                    break;
                case FieldType.DATETIME:
                    DateFormat df1 = new SimpleDateFormat(DATETIME_FORMAT);
                    if (!oldFieldValues.isEmpty()) {
                        create.setOldString(df1.format(oldFieldValues.get(0).getDateValue()));
                    }
                    if (!newFieldValues.isEmpty()) {
                        create.setNewString(df1.format(newFieldValues.get(0).getDateValue()));
                    }
                    break;
                case FieldType.DATE:
                    DateFormat df3 = new SimpleDateFormat(DATE_FORMAT);
                    if (!oldFieldValues.isEmpty()) {
                        create.setOldString(df3.format(oldFieldValues.get(0).getDateValue()));
                    }
                    if (!newFieldValues.isEmpty()) {
                        create.setNewString(df3.format(newFieldValues.get(0).getDateValue()));
                    }
                    break;
                case FieldType.TIME:
                    DateFormat df2 = new SimpleDateFormat(DATETIME_FORMAT);
                    if (!oldFieldValues.isEmpty()) {
                        create.setOldString(df2.format(oldFieldValues.get(0).getDateValue()));
                    }
                    if (!newFieldValues.isEmpty()) {
                        create.setNewString(df2.format(newFieldValues.get(0).getDateValue()));
                    }
                    break;
                case FieldType.INPUT:
                    if (!oldFieldValues.isEmpty()) {
                        create.setOldString(oldFieldValues.get(0).getStringValue());
                    }
                    if (!newFieldValues.isEmpty()) {
                        create.setNewString(newFieldValues.get(0).getStringValue());
                    }
                    break;
                case FieldType.NUMBER:
                    if (!oldFieldValues.isEmpty()) {
                        create.setOldString(oldFieldValues.get(0).getNumberValue());
                    }
                    if (!newFieldValues.isEmpty()) {
                        create.setNewString(newFieldValues.get(0).getNumberValue());
                    }
                    break;
                case FieldType.TEXT:
                    if (!oldFieldValues.isEmpty()) {
                        create.setOldString(oldFieldValues.get(0).getTextValue());
                    }
                    if (!newFieldValues.isEmpty()) {
                        create.setNewString(newFieldValues.get(0).getTextValue());
                    }
                    break;
                case FieldType.MULTI_MEMBER:
                case FieldType.MEMBER:
                    //查询用户
                    List<Long> userIds = oldFieldValues.stream().map(FieldValueDTO::getOptionId).collect(Collectors.toList());
                    userIds.addAll(newFieldValues.stream().map(FieldValueDTO::getOptionId).collect(Collectors.toList()));
                    Map<Long, UserDTO> userMap = handleUserMap(userIds);
                    if (!oldFieldValues.isEmpty()) {
                        create.setOldValue(oldFieldValues.stream().map(oldFieldValue -> String.valueOf(oldFieldValue.getOptionId())).collect(Collectors.joining(",")));
                        create.setOldString(handlerStringValue(oldFieldValues.stream().map(FieldValueDTO::getOptionId).collect(Collectors.toList()),userMap));
                    }
                    if (!newFieldValues.isEmpty()) {
                        create.setNewValue(newFieldValues.stream().map(newFieldValue -> String.valueOf(newFieldValue.getOptionId())).collect(Collectors.joining(",")));
                        create.setNewString(handlerStringValue(newFieldValues.stream().map(FieldValueDTO::getOptionId).collect(Collectors.toList()),userMap));
                    }
                    break;
                default:
                    break;
            }
        } catch (Exception e) {
            throw new CommonException("error", e);
        }
        if (checkCreateFieldDataLog(create)) {
            fieldDataLogService.createDataLog(projectId, schemeCode, create);
        }
    }

    private static boolean checkCreateFieldDataLog(FieldDataLogCreateVO create) {
        String oldString = create.getOldString();
        String newString = create.getNewString();
        if (!(Objects.isNull(oldString) && Objects.isNull(newString))
                && !Objects.equals(oldString, newString)) {
            return true;
        }
        return false;
    }

    private static String handlerStringValue(List<Long> userIds, Map<Long, UserDTO> userMap) {
        List<String> stringList = new ArrayList<>();
        userIds.forEach(userId -> stringList.add(userMap.getOrDefault(userId,new UserDTO()).getRealName()));
        return  stringList.stream().collect(Collectors.joining(","));
    }

    public static void handleAgileSortPageRequest(String fieldCode, String fieldType, PageRequest pageRequest) {
        try {
            Map<String, String> order = new HashMap<>(1);
            switch (fieldType) {
                case FieldType.DATETIME:
                case FieldType.DATE:
                case FieldType.TIME:
                    order.put(fieldCode, DATE_VALUE);
                    break;
                case FieldType.RADIO:
                case FieldType.SINGLE:
                case FieldType.MEMBER:
                    order.put(fieldCode, "option_id");
                    break;
                case FieldType.INPUT:
                    order.put(fieldCode, "string_value");
                    break;
                case FieldType.TEXT:
                    order.put(fieldCode, "text_value");
                    break;
                case FieldType.NUMBER:
                    order.put(fieldCode, "number_value");
                    break;
                default:
                    break;
            }
            PageUtil.sortResetOrder(pageRequest.getSort(), "fv", order);
        } catch (Exception e) {
            throw new CommonException("error", e);
        }
    }

    public static List<FieldDataLogCreateVO> batchHandlerFiledLog(Long projectId,Long instanceId,List<FieldValueDTO> oldFieldValues, List<FieldValueDTO> newFieldValues){
        List<FieldDataLogCreateVO> list = new ArrayList<>();
        Map<Long, List<FieldValueDTO>> oldFieldMap = new HashMap<>();
        if(!CollectionUtils.isEmpty(oldFieldValues)){
            oldFieldMap.putAll(oldFieldValues.stream().collect(Collectors.groupingBy(FieldValueDTO::getFieldId)));
        }
        Map<Long, List<FieldValueDTO>> newFieldMap = new HashMap<>();
        if(!CollectionUtils.isEmpty(newFieldValues)){
            newFieldMap.putAll(newFieldValues.stream().collect(Collectors.groupingBy(FieldValueDTO::getFieldId)));
        }
        Iterator<Map.Entry<Long, List<FieldValueDTO>>> it = newFieldMap.entrySet().iterator();
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        while (it.hasNext()){
            Map.Entry<Long, List<FieldValueDTO>> next = it.next();
            List<FieldValueDTO> oldValue =CollectionUtils.isEmpty(oldFieldMap.get(next.getKey())) ? new ArrayList<>() : oldFieldMap.get(next.getKey());
            List<FieldValueDTO> value = next.getValue();
            if(!CollectionUtils.isEmpty(value)){
                FieldDataLogCreateVO fieldDataLogCreateVO = new FieldDataLogCreateVO();
                fieldDataLogCreateVO.setFieldId(next.getKey());
                fieldDataLogCreateVO.setInstanceId(instanceId);
                batchHandlerFiled(fieldDataLogCreateVO,organizationId,value.get(0).getFieldType(),oldValue,value);
                list.add(fieldDataLogCreateVO);
            }
        }
        return list;
    }
    public static void batchHandlerFiled(FieldDataLogCreateVO create,Long organizationId,String fieldType, List<FieldValueDTO> oldFieldValues, List<FieldValueDTO> newFieldValues){
        FieldOptionMapper fieldOptionMapper = SpringBeanUtil.getBean(FieldOptionMapper.class);
        try {
            switch (fieldType) {
                case FieldType.CHECKBOX:
                case FieldType.MULTIPLE:
                    if (!oldFieldValues.isEmpty()) {
                        create.setOldValue(oldFieldValues.stream().map(x -> String.valueOf(x.getOptionId())).collect(Collectors.joining(",")));
                        create.setOldString(oldFieldValues.stream().map(FieldValueDTO::getOptionValue).collect(Collectors.joining(",")));
                    }
                    if (!newFieldValues.isEmpty()) {
                        List<Long> newOptionIds = newFieldValues.stream().map(FieldValueDTO::getOptionId).collect(Collectors.toList());
                        create.setNewValue(newOptionIds.stream().map(String::valueOf).collect(Collectors.joining(",")));
                        create.setNewString(fieldOptionMapper.selectByOptionIds(organizationId, newOptionIds).stream().map(FieldOptionDTO::getValue).collect(Collectors.joining(",")));
                    }
                    break;
                case FieldType.RADIO:
                case FieldType.SINGLE:
                    if (!oldFieldValues.isEmpty()) {
                        create.setOldValue(String.valueOf(oldFieldValues.get(0).getOptionId()));
                        create.setOldString(String.valueOf(oldFieldValues.get(0).getOptionValue()));
                    }
                    if (!newFieldValues.isEmpty()) {
                        List<Long> newOptionIds = Arrays.asList(newFieldValues.get(0).getOptionId());
                        List<FieldOptionDTO> fieldOptions = fieldOptionMapper.selectByOptionIds(organizationId, newOptionIds);
                        create.setNewValue(String.valueOf(newFieldValues.get(0).getOptionId()));
                        if (!fieldOptions.isEmpty()) {
                            create.setNewString(fieldOptions.get(0).getValue());
                        }
                    }
                    break;
                case FieldType.DATETIME:
                    DateFormat df1 = new SimpleDateFormat(DATETIME_FORMAT);
                    if (!oldFieldValues.isEmpty()) {
                        create.setOldString(df1.format(oldFieldValues.get(0).getDateValue()));
                    }
                    if (!newFieldValues.isEmpty()) {
                        create.setNewString(df1.format(newFieldValues.get(0).getDateValue()));
                    }
                    break;
                case FieldType.DATE:
                    DateFormat df3 = new SimpleDateFormat(DATE_FORMAT);
                    if (!oldFieldValues.isEmpty()) {
                        create.setOldString(df3.format(oldFieldValues.get(0).getDateValue()));
                    }
                    if (!newFieldValues.isEmpty()) {
                        create.setNewString(df3.format(newFieldValues.get(0).getDateValue()));
                    }
                    break;
                case FieldType.TIME:
                    DateFormat df2 = new SimpleDateFormat(DATETIME_FORMAT);
                    if (!oldFieldValues.isEmpty()) {
                        create.setOldString(df2.format(oldFieldValues.get(0).getDateValue()));
                    }
                    if (!newFieldValues.isEmpty()) {
                        create.setNewString(df2.format(newFieldValues.get(0).getDateValue()));
                    }
                    break;
                case FieldType.INPUT:
                    if (!oldFieldValues.isEmpty()) {
                        create.setOldString(oldFieldValues.get(0).getStringValue());
                    }
                    if (!newFieldValues.isEmpty()) {
                        create.setNewString(newFieldValues.get(0).getStringValue());
                    }
                    break;
                case FieldType.NUMBER:
                    if (!oldFieldValues.isEmpty()) {
                        create.setOldString(oldFieldValues.get(0).getNumberValue());
                    }
                    if (!newFieldValues.isEmpty()) {
                        create.setNewString(newFieldValues.get(0).getNumberValue());
                    }
                    break;
                case FieldType.TEXT:
                    if (!oldFieldValues.isEmpty()) {
                        create.setOldString(oldFieldValues.get(0).getTextValue());
                    }
                    if (!newFieldValues.isEmpty()) {
                        create.setNewString(newFieldValues.get(0).getTextValue());
                    }
                    break;
                case FieldType.MEMBER:
                    //查询用户
                    List<Long> userIds = oldFieldValues.stream().map(FieldValueDTO::getOptionId).collect(Collectors.toList());
                    userIds.addAll(newFieldValues.stream().map(FieldValueDTO::getOptionId).collect(Collectors.toList()));
                    Map<Long, UserDTO> userMap = handleUserMap(userIds);
                    if (!oldFieldValues.isEmpty()) {
                        create.setOldValue(String.valueOf(oldFieldValues.get(0).getOptionId()));
                        create.setOldString(userMap.getOrDefault(oldFieldValues.get(0).getOptionId(), new UserDTO()).getRealName());
                    }
                    if (!newFieldValues.isEmpty()) {
                        create.setNewValue(String.valueOf(newFieldValues.get(0).getOptionId()));
                        create.setNewString(userMap.getOrDefault(newFieldValues.get(0).getOptionId(), new UserDTO()).getRealName());
                    }
                    break;
                default:
                    break;
            }
        } catch (Exception e) {
            throw new CommonException("error.batch.handle.fields", e);
        }
    }


}