package io.choerodon.agile.app.service.v2.impl;

import java.io.IOException;
import java.math.BigDecimal;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.exc.InvalidFormatException;
import org.apache.commons.collections4.SetUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.Assert;

import io.choerodon.agile.api.vo.business.TagVO;
import io.choerodon.agile.api.vo.search.*;
import io.choerodon.agile.app.service.v2.FixPersonalFilterService;
import io.choerodon.agile.infra.dto.PersonalFilterDTO;
import io.choerodon.agile.infra.enums.FieldCode;
import io.choerodon.agile.infra.enums.PersonalFilterTypeCode;
import io.choerodon.agile.infra.enums.search.SearchConstant;
import io.choerodon.agile.infra.mapper.PersonalFilterMapper;
import io.choerodon.agile.infra.utils.EncryptionUtils;

import static io.choerodon.agile.infra.enums.search.SearchConstant.Operation;
import static io.choerodon.agile.infra.enums.search.SearchConstant.Relationship;

import org.hzero.core.base.BaseConstants;
import org.hzero.core.util.JsonUtils;
import org.hzero.core.util.Pair;
import org.hzero.starter.keyencrypt.core.IEncryptionService;

/**
 * @author superlee
 * @since 2022-11-30
 */
@Service
public class FixPersonalFilterServiceImpl implements FixPersonalFilterService {

    private static final Logger logger = LoggerFactory.getLogger(FixPersonalFilterServiceImpl.class);
    @Autowired
    private ObjectMapper objectMapper;
    @Autowired
    private PersonalFilterMapper personalFilterMapper;
    @Autowired
    private IEncryptionService iEncryptionService;

    public static final String TYPE_BACKLOG = "backlog";

    private static final List<String> CONDITION_KEYS =
            Arrays.asList("advancedSearchArgs", "otherArgs", "searchArgs");

    private static final List<String> PREDEFINED_NUMBER_FIELDS = Arrays.asList(
            FieldCode.STORY_POINTS,
            FieldCode.ESTIMATE_TIME,
            FieldCode.REMAINING_TIME,
            FieldCode.PROGRESS);

    private static final String CUSTOM_FIELD = "customField";
    private static final String TAGS = "tags";

    private static final String YQ_CLOUD_FLAG = "yqCloudFlag";
    private static final String YQ_CLOUD_NUM = "yqCloudNum";
    private static final String ISSUE_TYPE_ID = "issueTypeId";
    private static final String STATUS_ID = "statusId";
    private static final String STATUS_IDS = "statusIds";
    private static final String STATUS_LIST = "statusList";
    private static final String REPORTER_IDS = "reporterIds";
    private static final String PRIORITY_ID = "priorityId";
    private static final String PRIORITY_IDS = "priorityIds";
    private static final String MAIN_RESPONSIBLE_IDS = "mainResponsibleIds";
    private static final String ASSIGNEE_ID = "assigneeId";
    private static final String UPDATOR_IDS = "updatorIds";
    private static final String PRODUCT_IDS = "productIds";
    private static final String CREATOR_IDS = "creatorIds";
    private static final String PARTICIPANT_IDS = "participantIds";
    private static final String UPDATE = "update";
    private static final String CREATE = "create";
    private static final String CONTENTS = "contents";
    private static final String CONTENT = "content";
    private static final String ISSUE_IDS = "issueIds";
    private static final String QUICK_FILTER_IDS = "quickFilterIds";
    private static final String STAR_BEACON = "starBeacon";
    private static final String MY_ASSIGNED = "myAssigned";
    private static final String USER_ID = "userId";
    private static final String USER_IDS = "userIds";
    private static final String TREE = "tree";
    private static final String TYPE_IDS = "typeIds";
    private static final String CLASSIFICATION_IDS = "classificationIds";


    private static final String SUFFIX_AGILE_START_DATE = "StartDate";
    private static final String SUFFIX_AGILE_END_DATE = "EndDate";
    private static final String SUFFIX_AGILE_SCOPE_START = "ScopeStart";
    private static final String SUFFIX_AGILE_SCOPE_END = "ScopeEnd";
    private static final String SUFFIX_FEATURE_FROM = "From";
    private static final String SUFFIX_FEATURE_TO = "To";

    private static final List<String> YQ_CLOUD_FIELDS = Arrays.asList(YQ_CLOUD_FLAG, YQ_CLOUD_NUM);

    private static final List<String> AGILE_DATE_SUFFIX = Arrays.asList(SUFFIX_AGILE_START_DATE, SUFFIX_AGILE_END_DATE, SUFFIX_AGILE_SCOPE_START, SUFFIX_AGILE_SCOPE_END);
    private static final List<String> FEATURE_DATE_SUFFIX = Arrays.asList(SUFFIX_AGILE_START_DATE, SUFFIX_AGILE_END_DATE, SUFFIX_FEATURE_FROM, SUFFIX_FEATURE_TO);

    private static final Map<String, String> AGILE_OPTION_FIELD_MAPPING = new HashMap<>();
    private static final Map<String, String> FEATURE_OPTION_FIELD_MAPPING = new HashMap<>();
    private static final Map<String, String> DATE_FIELD_MAPPING = new HashMap<>();

    private static final Map<String, String> BACKLOG_OPTION_FIELD_MAPPING = new HashMap<>();

    private static final String OPTION = "option";
    private static final String DATE = "date";
    private static final String DATE_HMS = "date_hms";
    private static final String NUMBER = "number";
    private static final String STRING = "string";
    private static final String TEXT = "text";

    private static final String FIELD_ID = "fieldId";
    private static final String VALUE = "value";
    private static final String START_DATE = "startDate";
    private static final String END_DATE = "endDate";

    private static final String ERROR_ILLEGAL_DATA = "存在加密值的非法数据，跳过此数据";

    private static final Map<String, Set<String>> IGNORED_FIELD_BY_TYPE_MAP = new HashMap<>();

    private static final String EPIC_LIST = "epicList";
    private static final String FEATURE_TYPE_LIST = "featureTypeList";
    private static final String SPRINT_LIST = "sprintList";
    private static final String ISSUE_TYPE_LIST = "issueTypeList";
    private static final String REPORTER_LIST = "reporterList";
    private static final String PI_LIST = "piList";
    private static final String TEAM_PROJECT_LIST = "teamProjectList";
    private static final String ASSIGNEE_IDS = "assigneeIds";


    static {
        //敏捷/瀑布字段映射
        AGILE_OPTION_FIELD_MAPPING.put(ISSUE_TYPE_ID, FieldCode.ISSUE_TYPE);
        AGILE_OPTION_FIELD_MAPPING.put(STATUS_ID, FieldCode.STATUS);
        AGILE_OPTION_FIELD_MAPPING.put(REPORTER_IDS, FieldCode.REPORTER);
        AGILE_OPTION_FIELD_MAPPING.put(PRIORITY_ID, FieldCode.PRIORITY);
        AGILE_OPTION_FIELD_MAPPING.put(MAIN_RESPONSIBLE_IDS, FieldCode.MAIN_RESPONSIBLE);
        AGILE_OPTION_FIELD_MAPPING.put(FieldCode.SPRINT, FieldCode.SPRINT);
        AGILE_OPTION_FIELD_MAPPING.put(FieldCode.EPIC, FieldCode.EPIC);
        AGILE_OPTION_FIELD_MAPPING.put(FieldCode.LABEL, FieldCode.LABEL);
        AGILE_OPTION_FIELD_MAPPING.put(FieldCode.FIX_VERSION, FieldCode.FIX_VERSION);
        AGILE_OPTION_FIELD_MAPPING.put(ASSIGNEE_ID, FieldCode.ASSIGNEE);
        AGILE_OPTION_FIELD_MAPPING.put(UPDATOR_IDS, FieldCode.UPDATOR);
        AGILE_OPTION_FIELD_MAPPING.put(FieldCode.COMPONENT, FieldCode.COMPONENT);
        AGILE_OPTION_FIELD_MAPPING.put(FieldCode.ENVIRONMENT, FieldCode.ENVIRONMENT);
        AGILE_OPTION_FIELD_MAPPING.put(FieldCode.FEATURE, FieldCode.FEATURE);
        AGILE_OPTION_FIELD_MAPPING.put(PRODUCT_IDS, FieldCode.PRODUCT);
        AGILE_OPTION_FIELD_MAPPING.put(FieldCode.INFLUENCE_VERSION, FieldCode.INFLUENCE_VERSION);
        AGILE_OPTION_FIELD_MAPPING.put(CREATOR_IDS, FieldCode.CREATOR);
        AGILE_OPTION_FIELD_MAPPING.put(PARTICIPANT_IDS, FieldCode.PARTICIPANT);
        //路线图特性列表字段映射
        FEATURE_OPTION_FIELD_MAPPING.put(STATUS_LIST, FieldCode.STATUS);
        FEATURE_OPTION_FIELD_MAPPING.put(SPRINT_LIST, FieldCode.SPRINT);
        FEATURE_OPTION_FIELD_MAPPING.put(EPIC_LIST, FieldCode.EPIC);
        FEATURE_OPTION_FIELD_MAPPING.put(FEATURE_TYPE_LIST, FieldCode.FEATURE_TYPE);
        FEATURE_OPTION_FIELD_MAPPING.put(ISSUE_TYPE_LIST, FieldCode.ISSUE_TYPE);
        FEATURE_OPTION_FIELD_MAPPING.put(REPORTER_LIST, FieldCode.REPORTER);
        FEATURE_OPTION_FIELD_MAPPING.put(PRODUCT_IDS, FieldCode.PRODUCT);
        FEATURE_OPTION_FIELD_MAPPING.put(CREATOR_IDS, FieldCode.CREATOR);
        FEATURE_OPTION_FIELD_MAPPING.put(PI_LIST, FieldCode.PI);
        FEATURE_OPTION_FIELD_MAPPING.put(FieldCode.PROGRAM_VERSION, FieldCode.PROGRAM_VERSION);
        FEATURE_OPTION_FIELD_MAPPING.put(UPDATOR_IDS, FieldCode.UPDATOR);
        FEATURE_OPTION_FIELD_MAPPING.put(TEAM_PROJECT_LIST, FieldCode.SUB_PROJECT);
        //日期范围字段映射
        DATE_FIELD_MAPPING.put(CREATE, FieldCode.CREATION_DATE);
        DATE_FIELD_MAPPING.put(UPDATE, FieldCode.LAST_UPDATE_DATE);
        DATE_FIELD_MAPPING.put(FieldCode.LAST_UPDATE_DATE, FieldCode.LAST_UPDATE_DATE);
        DATE_FIELD_MAPPING.put(FieldCode.ACTUAL_START_TIME, FieldCode.ACTUAL_START_TIME);
        DATE_FIELD_MAPPING.put(FieldCode.ACTUAL_END_TIME, FieldCode.ACTUAL_END_TIME);
        DATE_FIELD_MAPPING.put(FieldCode.ESTIMATED_START_TIME, FieldCode.ESTIMATED_START_TIME);
        DATE_FIELD_MAPPING.put(FieldCode.ESTIMATED_END_TIME, FieldCode.ESTIMATED_END_TIME);
        //需求字段映射
        BACKLOG_OPTION_FIELD_MAPPING.put(PRIORITY_IDS, FieldCode.PRIORITY);
        BACKLOG_OPTION_FIELD_MAPPING.put(USER_IDS, FieldCode.PROCESSOR);
        BACKLOG_OPTION_FIELD_MAPPING.put(TYPE_IDS, SearchConstant.Field.TYPE_ID);
        BACKLOG_OPTION_FIELD_MAPPING.put(CREATOR_IDS, FieldCode.CREATOR);
        BACKLOG_OPTION_FIELD_MAPPING.put(SearchConstant.Field.SOURCE, SearchConstant.Field.SOURCE);
        BACKLOG_OPTION_FIELD_MAPPING.put(CLASSIFICATION_IDS, FieldCode.BACKLOG_CLASSIFICATION);
        BACKLOG_OPTION_FIELD_MAPPING.put(STATUS_IDS, FieldCode.STATUS);
        BACKLOG_OPTION_FIELD_MAPPING.put(UPDATOR_IDS, FieldCode.UPDATOR);


        //todo teamProjectList reporterList 代明确
        IGNORED_FIELD_BY_TYPE_MAP.put(PersonalFilterTypeCode.AGILE_ISSUE,
                SetUtils.unmodifiableSet(
                        ASSIGNEE_IDS,
                        "version",
                        FieldCode.REPORTER,
                        FieldCode.SUMMARY,
                        FieldCode.ASSIGNEE,
                        FieldCode.ISSUE_NUM,
                        USER_ID,
                        TEAM_PROJECT_LIST,
                        REPORTER_LIST,
                        SPRINT_LIST,
                        PI_LIST));
        IGNORED_FIELD_BY_TYPE_MAP.put(PersonalFilterTypeCode.FEATURE_ISSUE,
                SetUtils.unmodifiableSet(
                        ASSIGNEE_IDS,
                        FieldCode.REPORTER,
                        FieldCode.SUMMARY,
                        FieldCode.ASSIGNEE,
                        FieldCode.ISSUE_NUM,
                        ASSIGNEE_ID, REPORTER_IDS,
                        STATUS_ID,
                        FieldCode.COMPONENT,
                        FieldCode.SPRINT,
                        ISSUE_TYPE_ID,
                        FieldCode.ENVIRONMENT));
        IGNORED_FIELD_BY_TYPE_MAP.put(TYPE_BACKLOG, SetUtils.unmodifiableSet(USER_ID));
    }


    @Override
    public void fix(Set<String> typeCodes) {
        logger.info("修复高级筛选数据，类型有：{}", typeCodes);
        //系统字段
        typeCodes.forEach(typeCode -> {
            PersonalFilterDTO dto = new PersonalFilterDTO();
            dto.setFilterTypeCode(typeCode);
            List<PersonalFilterDTO> personalFilters = personalFilterMapper.select(dto);
            personalFilters.forEach(filter -> {
                String json = filter.getFilterJson();
                if (StringUtils.isEmpty(json) || !StringUtils.isEmpty(filter.getAdvancedFilterJson())) {
                    return;
                }
                String advancedFilterJson = convertJsonToAdvancedFilterJson(typeCode, json);
                if (advancedFilterJson == null) {
                    return;
                }
                filter.setAdvancedFilterJson(advancedFilterJson);
                personalFilterMapper.updateByPrimaryKeySelective(filter);
            });
        });
        logger.info("修复高级筛选数据完成");
    }

    @Override
    public String convertJsonToAdvancedFilterJson(String typeCode, String json) {
        SearchParamVO searchParamVO = new SearchParamVO();
        try {
            JsonNode jsonNode = objectMapper.readTree(json);
            Iterator<String> fieldNameIterator = jsonNode.fieldNames();
            while (fieldNameIterator.hasNext()) {
                String fieldName = fieldNameIterator.next();
                JsonNode conditionNode = jsonNode.get(fieldName);
                if (CONDITION_KEYS.contains(fieldName)) {
                    processConditionDetails(conditionNode, searchParamVO, typeCode);
                } else if (CONTENTS.equals(fieldName) || CONTENT.equals(fieldName)) {
                    processContents(searchParamVO, conditionNode);
                } else if (QUICK_FILTER_IDS.equals(fieldName)) {
                    //快速筛选
                    parseQuickFilterIds(searchParamVO, fieldName, conditionNode);
                } else {
                    logger.error("不合法的字段名: {}", fieldName);
                }
            }
        } catch (IOException e) {
            logger.error("不合法json数据: {}", json);
            return null;
        }
        return JsonUtils.toJson(searchParamVO);
    }

    private void parseQuickFilterIds(SearchParamVO searchParamVO, String fieldName, JsonNode conditionNode) throws IOException {
        JsonNode option = conditionNode.get(fieldName);
        if (option != null && !option.isNull() && !option.isEmpty()) {
            List<Long> valueIds = objectMapper.readValue(option.traverse(), new TypeReference<List<Long>>() {
            });
            searchParamVO.setQuickFilterIds(valueIds);
        }
    }

    private void processContents(SearchParamVO searchParamVO, JsonNode conditionNode) {
        Iterator<JsonNode> contentIterator = conditionNode.iterator();
        //只取第一个content
        String content = null;
        while (contentIterator.hasNext()) {
            content = contentIterator.next().asText();
            if (content != null) {
                break;
            }
        }
        if (content != null) {
            List<Condition> conditions = Optional.ofNullable(searchParamVO.getConditions()).orElse(new ArrayList<>());
            searchParamVO.setConditions(conditions);
            conditions.add(
                    new Condition()
                            .setField(new Field().setFieldCode(SearchConstant.Field.CONTENT).setPredefined(true))
                            .setRelationship(Relationship.AND.toString())
                            .setOperation(Operation.LIKE.toString())
                            .setValue(new Value().setValueStr(content)));
        }
    }

    private void processConditionDetails(JsonNode conditionNode,
                                         SearchParamVO searchParamVO,
                                         String typeCode) throws IOException {
        List<Condition> conditions = Optional.ofNullable(searchParamVO.getConditions()).orElse(new ArrayList<>());
        searchParamVO.setConditions(conditions);
        Iterator<String> conditionFieldNameIterator = conditionNode.fieldNames();
        boolean yqCloudFieldDone = false;
        Map<String, Boolean> dateFieldDoneMap = new HashMap<>();
        Map<String, String> optionMap = new HashMap<>();
        switch (typeCode) {
            case PersonalFilterTypeCode.AGILE_ISSUE:
            case PersonalFilterTypeCode.WATERFALL_ISSUE:
                optionMap = AGILE_OPTION_FIELD_MAPPING;
                break;
            case PersonalFilterTypeCode.FEATURE_ISSUE:
                optionMap = FEATURE_OPTION_FIELD_MAPPING;
                break;
            case TYPE_BACKLOG:
                optionMap = BACKLOG_OPTION_FIELD_MAPPING;
                break;
            default:
                break;
        }


        while (conditionFieldNameIterator.hasNext()) {
            String conditionFieldName = conditionFieldNameIterator.next();
            if (PREDEFINED_NUMBER_FIELDS.contains(conditionFieldName)) {
                //数字类型字段
                processNumberField(conditionNode, searchParamVO, conditions, conditionFieldName);
            } else if (YQ_CLOUD_FIELDS.contains(conditionFieldName)) {
                if (yqCloudFieldDone) {
                    continue;
                }
                processYqCloudField(conditionNode, conditions);
                yqCloudFieldDone = true;
            } else if (CUSTOM_FIELD.equals(conditionFieldName)) {
                //自定义字段
                processCustomField(conditionNode, searchParamVO, conditions, conditionFieldName);
            } else if (TAGS.equals(conditionFieldName)) {
                //tag需要特殊处理
                processTagField(conditionNode, searchParamVO, conditions, conditionFieldName);
            } else if (ISSUE_IDS.equals(conditionFieldName)) {
                processIssueIds(conditionNode, searchParamVO, conditionFieldName);
            } else if (STAR_BEACON.equals(conditionFieldName) || MY_ASSIGNED.equals(conditionFieldName)) {
                //星标
                processStarBeaconOrMyAssigned(conditionNode, conditionFieldName, conditions);
            } else if (TREE.equals(conditionFieldName)) {
                //树形展示
                processTree(conditionNode, searchParamVO, conditionFieldName);
            } else if (optionMap.keySet().contains(conditionFieldName)) {
                //下拉框
                processOption(conditionNode, searchParamVO, conditions, conditionFieldName, optionMap);
            } else if (FieldCode.ACCEPTANCE_CRITERA.equals(conditionFieldName)
                    || FieldCode.BENFIT_HYPOTHESIS.equals(conditionFieldName)) {
                //特性字符串
                processFeatureStringField(conditionNode, conditions, conditionFieldName);
            } else {
                //日期
                processDate(conditionNode, conditions, dateFieldDoneMap, conditionFieldName, typeCode);
            }
        }
    }

    private void processTree(JsonNode conditionNode, SearchParamVO searchParamVO, String conditionFieldName) {
        JsonNode node = conditionNode.get(conditionFieldName);
        if (node != null && node.isBoolean()) {
            boolean isTree = node.booleanValue();
            searchParamVO.setTreeFlag(isTree);
        }
    }

    private void processFeatureStringField(JsonNode conditionNode,
                                           List<Condition> conditions,
                                           String conditionFieldName) {
        JsonNode node = conditionNode.get(conditionFieldName);
        if (node != null && !node.isNull()) {
            String value = node.asText();
            Condition condition =
                    new Condition()
                            .setField(new Field().setFieldCode(conditionFieldName).setPredefined(true))
                            .setRelationship(Relationship.AND.toString())
                            .setOperation(Operation.LIKE.toString()).setValue(new Value().setValueStr(value));
            conditions.add(condition);
        }
    }

    private void processStarBeaconOrMyAssigned(JsonNode conditionNode,
                                               String conditionFieldName,
                                               List<Condition> conditions) {
        JsonNode node = conditionNode.get(conditionFieldName);
        if (node != null && node.isBoolean()) {
            boolean star = Boolean.TRUE.equals(node.booleanValue());
            if (star) {
                JsonNode userIdNode = conditionNode.get(USER_ID);
                if (userIdNode != null && !userIdNode.isNull()) {
                    String userIdStr = userIdNode.asText();
                    Long userId;
                    String fieldCode = SearchConstant.Field.MY_STAR;
                    if (MY_ASSIGNED.equals(conditionFieldName)) {
                        fieldCode = SearchConstant.Field.MY_PARTICIPATE;
                    }
                    if (StringUtils.isNumeric(userIdStr)) {
                        userId = Long.parseLong(userIdStr);
                    } else {
                        try {
                            //非法加密值，尝试解密
                            String str = iEncryptionService.decrypt(userIdStr, EncryptionUtils.BLANK_KEY, null, true);
                            userId = Long.parseLong(str);
                        } catch (Exception e) {
                            logger.warn(ERROR_ILLEGAL_DATA);
                            return;
                        }
                    }
                    Condition condition =
                            new Condition()
                                    .setField(new Field().setFieldCode(fieldCode).setPredefined(true))
                                    .setRelationship(Relationship.AND.toString())
                                    .setOperation(Operation.IN.toString())
                                    .setValue(new Value().setValueIdList(Arrays.asList(userId)));
                    conditions.add(condition);
                }
            }
        }
    }

    private void processIssueIds(JsonNode conditionNode, SearchParamVO searchParamVO, String conditionFieldName) throws IOException {
        JsonNode option = conditionNode.get(conditionFieldName);
        if (option != null && !option.isNull() && !option.isEmpty()) {
            Set<Long> issueIds = objectMapper.readValue(option.traverse(), new TypeReference<Set<Long>>() {
            });
            searchParamVO.setInstanceIds(issueIds);
        }
    }

    private void processDate(JsonNode conditionNode,
                             List<Condition> conditions,
                             Map<String, Boolean> dateFieldDoneMap,
                             String conditionFieldName,
                             String typeCode) {
        Set<String> ignoredFields = Optional.ofNullable(IGNORED_FIELD_BY_TYPE_MAP.get(typeCode)).orElse(new HashSet<>());
        String field = null;
        List<String> dateSuffix = new ArrayList<>();
        String startSuffix = SUFFIX_AGILE_SCOPE_START;
        String endSuffix = SUFFIX_AGILE_SCOPE_END;
        switch (typeCode) {
            case PersonalFilterTypeCode.AGILE_ISSUE:
            case PersonalFilterTypeCode.WATERFALL_ISSUE:
            case TYPE_BACKLOG:
                dateSuffix = AGILE_DATE_SUFFIX;
                break;
            case PersonalFilterTypeCode.FEATURE_ISSUE:
                dateSuffix = FEATURE_DATE_SUFFIX;
                startSuffix = SUFFIX_FEATURE_FROM;
                endSuffix = SUFFIX_FEATURE_TO;
                break;
            default:
                break;
        }
        for (String suffix : dateSuffix) {
            if (conditionFieldName.endsWith(suffix)) {
                field = conditionFieldName.substring(0, conditionFieldName.length() - suffix.length());
                break;
            }
        }
        if (field == null) {
            //非日期字段，抛异常
            if (!ignoredFields.contains(conditionFieldName)) {
                logger.error("不合法的json key: {}", conditionFieldName);
            }
            return;
        }
        //判断字段是否已经处理过
        boolean done = Boolean.TRUE.equals(dateFieldDoneMap.get(field));
        if (!done) {
            dateFieldDoneMap.put(field, true);
            Pair<Value, Value> pair = null;
            String pattern = BaseConstants.Pattern.DATETIME;
            if (CREATE.equals(field) || UPDATE.equals(field)) {
                JsonNode start = conditionNode.get(field + SUFFIX_AGILE_START_DATE);
                JsonNode end = conditionNode.get(field + SUFFIX_AGILE_END_DATE);
                if (!isDateRangeIllegal(start, end)) {
                    pair = generatePairDate(start, end, pattern);
                }
            } else {
                JsonNode start = conditionNode.get(field + startSuffix);
                JsonNode end = conditionNode.get(field + endSuffix);
                if (start != null && !start.isNull() && end != null && !end.isNull()) {
                    pair = generatePairDate(start, end, pattern);
                }
            }
            if (pair != null) {
                String fieldCode = DATE_FIELD_MAPPING.get(field);
                if (fieldCode == null) {
                    logger.error("不合法的日期字段:{}", field);
                    return;
                }
                Condition condition =
                        new Condition()
                                .setField(new Field().setFieldCode(fieldCode).setPredefined(true))
                                .setRelationship(Relationship.AND.toString())
                                .setOperation(Operation.BETWEEN.toString())
                                .setBetweenValues(pair);
                conditions.add(condition);
            }
        }
    }

    private void processCustomField(JsonNode conditionNode,
                                    SearchParamVO searchParamVO,
                                    List<Condition> conditions,
                                    String conditionFieldName) {
        JsonNode node = conditionNode.get(conditionFieldName);
        Iterator<String> iterator = node.fieldNames();
        while (iterator.hasNext()) {
            String prop = iterator.next();
            JsonNode valueList = node.get(prop);
            if (valueList == null || valueList.isNull() || valueList.isEmpty()) {
                continue;
            }
            Iterator<JsonNode> valueNodeIterator = valueList.iterator();
            while (valueNodeIterator.hasNext()) {
                JsonNode valueNode = valueNodeIterator.next();
                JsonNode idNode = valueNode.get(FIELD_ID);
                if (idNode == null || idNode.isNull()) {
                    continue;
                }
                Long fieldId = idNode.asLong();
                String relationship = Relationship.AND.toString();
                String opt = Operation.IN.toString();
                Pair<Value, Value> pair = null;
                JsonNode value = valueNode.get(VALUE);
                switch (prop) {
                    case OPTION:
                        pair = readCustomFieldOptionValue(pair, value);
                        break;
                    case NUMBER:
                        opt = Operation.EQUAL.toString();
                        pair = readCustomFieldNumberValue(pair, value);
                        break;
                    case STRING:
                    case TEXT:
                        opt = Operation.LIKE.toString();
                        pair = readCustomFieldStringValue(pair, value);
                        break;
                    case DATE:
                    case DATE_HMS:
                        opt = Operation.BETWEEN.toString();
                        pair = readCustomFieldDateValue(prop, valueNode, pair);
                        break;
                    default:
                        break;
                }
                if (pair == null) {
                    return;
                }
                Value first = pair.getFirst();
                Value second = pair.getSecond();
                if (first == null && second == null) {
                    //只存了字段id,用于界面展示
                    addEmptyCondition(searchParamVO, null, fieldId);
                } else if (second == null) {
                    Condition condition =
                            new Condition()
                                    .setField(new Field().setFieldId(fieldId).setPredefined(false))
                                    .setRelationship(relationship)
                                    .setOperation(opt)
                                    .setValue(first);
                    conditions.add(condition);
                } else {
                    Condition condition =
                            new Condition()
                                    .setField(new Field().setFieldId(fieldId).setPredefined(false))
                                    .setRelationship(relationship)
                                    .setOperation(opt)
                                    .setBetweenValues(pair);
                    conditions.add(condition);
                }
            }
        }
    }

    private boolean isDateRangeIllegal(JsonNode start,
                                       JsonNode end) {
        return start == null || start.isNull() || StringUtils.isEmpty(start.asText()) ||
                end == null || end.isNull() || StringUtils.isEmpty(end.asText());
    }

    private Pair<Value, Value> readCustomFieldDateValue(String prop, JsonNode valueNode, Pair<Value, Value> pair) {
        JsonNode start = valueNode.get(START_DATE);
        JsonNode end = valueNode.get(END_DATE);
        if (!isDateRangeIllegal(start, end)) {
            String pattern = BaseConstants.Pattern.DATETIME;
            if (DATE_HMS.equals(prop)) {
                pattern = BaseConstants.Pattern.TIME_SS;
            }
            pair = generatePairDate(start, end, pattern);
        }
        return pair;
    }

    private Pair<Value, Value> generatePairDate(JsonNode start, JsonNode end, String pattern) {
        Pair<Value, Value> pair;
        SimpleDateFormat formatter = new SimpleDateFormat(pattern);
        try {
            Date startDate = parseDateFromString(start, formatter);
            Date endDate = parseDateFromString(end, formatter);
            pair = Pair.of(new Value().setValueDate(startDate), new Value().setValueDate(endDate));
        } catch (ParseException e) {
            logger.error("不合法的日期格式:{}", e.getMessage());
            return null;
        }
        return pair;
    }

    private Date parseDateFromString(JsonNode node, SimpleDateFormat formatter) throws ParseException {
        Date startDate;
        String startStr = node.asText();
        if (StringUtils.isNumeric(startStr)) {
            //处理脏数据
            startDate = new Date(Long.parseLong(startStr));
        } else {
            startDate = formatter.parse(node.asText());
        }
        return startDate;
    }

    private Pair<Value, Value> readCustomFieldStringValue(Pair<Value, Value> pair, JsonNode value) {
        if (value != null && !value.isNull()) {
            String valueStr = value.asText();
            pair = Pair.of(new Value().setValueStr(valueStr), null);
        }
        return pair;
    }

    private Pair<Value, Value> readCustomFieldNumberValue(Pair<Value, Value> pair, JsonNode value) {
        if (value != null && !value.isNull()) {
            BigDecimal number = new BigDecimal(value.asText());
            pair = Pair.of(new Value().setValueDecimal(number), null);
        }
        return pair;
    }

    private Pair<Value, Value> readCustomFieldOptionValue(Pair<Value, Value> pair, JsonNode value) {
        if (value != null && !value.isNull() && !value.isEmpty()) {
            List<Long> valueIdList = null;
            try {
                valueIdList = objectMapper.readValue(value.traverse(), new TypeReference<List<Long>>() {
                });
            } catch (IOException e) {
                logger.error("自定义字段选择器值映射错误: {}", e.getMessage());
                return null;
            }
            pair = Pair.of(new Value().setValueIdList(valueIdList), null);
        }
        return pair;
    }

    private void processOption(JsonNode conditionNode,
                               SearchParamVO searchParamVO,
                               List<Condition> conditions,
                               String conditionFieldName,
                               Map<String, String> optionMap) {
        JsonNode option = conditionNode.get(conditionFieldName);
        String fieldCode = optionMap.get(conditionFieldName);
        Assert.notNull(fieldCode, "error.field.code.null");
        if (option == null || option.isNull() || option.isEmpty()) {
            addEmptyCondition(searchParamVO, fieldCode, null);
        } else {
            Condition condition;
            try {
                if (FieldCode.ENVIRONMENT.equals(fieldCode) || FieldCode.FEATURE_TYPE.equals(fieldCode) || SearchConstant.Field.SOURCE.equals(fieldCode)) {
                    //环境，字符串类型数据
                    List<String> valueStrList = objectMapper.readValue(option.traverse(), new TypeReference<List<String>>() {
                    });
                    condition =
                            new Condition()
                                    .setField(new Field().setFieldCode(fieldCode).setPredefined(true))
                                    .setRelationship(Relationship.AND.toString())
                                    .setOperation(Operation.IN.toString())
                                    .setValue(new Value().setValueStrList(valueStrList));
                } else if (FieldCode.SUB_PROJECT.equals(fieldCode)) {
                    List<Long> valueIds = objectMapper.readValue(option.traverse(), new TypeReference<List<Long>>() {
                    });
                    condition =
                            new Condition()
                                    .setField(new Field().setFieldCode(fieldCode).setPredefined(true))
                                    .setRelationship(Relationship.AND.toString())
                                    .setOperation(Operation.IN.toString())
                                    .setValue(new Value().setNoEncryptIdList(valueIds));
                } else {
                    List<Long> valueIds;
                    try {
                        valueIds = objectMapper.readValue(option.traverse(), new TypeReference<List<Long>>() {
                        });
                    } catch (InvalidFormatException e) {
                        //非法加密值，尝试解密
                        try {
                            List<String> valueIdStr = objectMapper.readValue(option.traverse(), new TypeReference<List<String>>() {
                            });
                            valueIds = new ArrayList<>();
                            for (String idStr : valueIdStr) {
                                if (StringUtils.isNumeric(idStr)) {
                                    valueIds.add(Long.parseLong(idStr));
                                } else {
                                    String str = iEncryptionService.decrypt(idStr, EncryptionUtils.BLANK_KEY, null, true);
                                    valueIds.add(Long.parseLong(str));
                                }
                            }
                        } catch (Exception exception) {
                            logger.warn(ERROR_ILLEGAL_DATA);
                            return;
                        }
                    }
                    boolean containsNull = valueIds.contains(0L);
                    valueIds.remove(0L);
                    if (!valueIds.isEmpty()) {
                        if (containsNull) {
                            //括号的情况
                            condition =
                                    new Condition()
                                            .setField(new Field().setFieldCode(fieldCode).setPredefined(true))
                                            .setRelationship(Relationship.AND.toString())
                                            .setOperation(Operation.BRACKET.toString())
                                            .setSubConditions(
                                                    Arrays.asList(
                                                            new Condition()
                                                                    .setField(new Field().setFieldCode(fieldCode).setPredefined(true))
                                                                    .setRelationship(Relationship.AND.toString())
                                                                    .setOperation(Operation.IN.toString())
                                                                    .setValue(new Value().setValueIdList(valueIds)),
                                                            new Condition()
                                                                    .setField(new Field().setFieldCode(fieldCode).setPredefined(true))
                                                                    .setRelationship(Relationship.OR.toString())
                                                                    .setOperation(Operation.IS_NULL.toString())));
                        } else {
                            condition =
                                    new Condition()
                                            .setField(new Field().setFieldCode(fieldCode).setPredefined(true))
                                            .setRelationship(Relationship.AND.toString())
                                            .setOperation(Operation.IN.toString())
                                            .setValue(new Value().setValueIdList(valueIds));
                        }
                    } else {
                        //只有为空条件
                        condition =
                                new Condition()
                                        .setField(new Field().setFieldCode(fieldCode).setPredefined(true))
                                        .setRelationship(Relationship.AND.toString())
                                        .setOperation(Operation.IS_NULL.toString());
                    }
                }
            } catch (IOException e) {
                logger.error("系统字段选择器值映射错误:{}", e.getMessage());
                return;
            }
            conditions.add(condition);
        }
    }

    private void processNumberField(JsonNode conditionNode,
                                    SearchParamVO searchParamVO,
                                    List<Condition> conditions,
                                    String conditionFieldName) {
        JsonNode number = conditionNode.get(conditionFieldName);
        String fieldCode = conditionFieldName;
        if (number == null || number.isNull()) {
            //只存储了筛选的列
            addEmptyCondition(searchParamVO, fieldCode, null);
        } else {
            String value = number.asText();
            BigDecimal bigDecimal = new BigDecimal(value);
            Condition condition =
                    new Condition()
                            .setField(new Field().setFieldCode(fieldCode).setPredefined(true))
                            .setRelationship(Relationship.AND.toString())
                            .setOperation(Operation.EQUAL.toString())
                            .setValue(new Value().setValueDecimal(bigDecimal));
            conditions.add(condition);
        }
    }

    private void processTagField(JsonNode conditionNode,
                                 SearchParamVO searchParamVO,
                                 List<Condition> conditions,
                                 String conditionFieldName) {
        JsonNode tag = conditionNode.get(conditionFieldName);
        String fieldCode = conditionFieldName;
        if (tag == null || tag.isNull()) {
            addEmptyCondition(searchParamVO, fieldCode, null);
        } else {
            try {
                List<TagVO> tags = objectMapper.readValue(tag.traverse(), new TypeReference<List<TagVO>>() {
                });
                Condition condition =
                        new Condition()
                                .setField(new Field().setFieldCode(fieldCode).setPredefined(true))
                                .setRelationship(Relationship.AND.toString())
                                .setOperation(Operation.IN.toString())
                                .setValue(new Value().setObjectList(tags));
                conditions.add(condition);
            } catch (IOException e) {
                logger.error("Tag字段选择器值映射错误:{}", e.getMessage());
            }
        }
    }

    private void processYqCloudField(JsonNode conditionNode, List<Condition> conditions) {
        JsonNode yqCloudFlagNode = conditionNode.get(YQ_CLOUD_FLAG);
        JsonNode yqCloudNumNode = conditionNode.get(YQ_CLOUD_NUM);
        if (yqCloudFlagNode != null) {
            boolean yqCloudFlag = yqCloudFlagNode.asBoolean();
            Operation operation = yqCloudFlag ? Operation.IS_NOT_NULL : Operation.IS_NULL;
            Condition condition =
                    new Condition()
                            .setField(new Field().setFieldCode(SearchConstant.Field.YQ_CLOUD_NUM).setPredefined(true))
                            .setRelationship(SearchConstant.Relationship.AND.toString())
                            .setOperation(operation.toString());
            conditions.add(condition);
        } else if (yqCloudNumNode != null) {
            String yqCloudNum = yqCloudNumNode.asText();
            Condition condition =
                    new Condition()
                            .setField(new Field().setFieldCode(SearchConstant.Field.YQ_CLOUD_NUM).setPredefined(true))
                            .setRelationship(SearchConstant.Relationship.AND.toString())
                            .setOperation(Operation.LIKE.toString())
                            .setValue(new Value().setValueStr(yqCloudNum));
            conditions.add(condition);
        }
    }

    private void addEmptyCondition(SearchParamVO searchParamVO,
                                   String fieldCode,
                                   Long fieldId) {
        EmptyCondition emptyCondition = Optional.ofNullable(searchParamVO.getEmptyCondition()).orElse(new EmptyCondition());
        searchParamVO.setEmptyCondition(emptyCondition);
        if (fieldCode != null) {
            List<String> predefinedFieldCode = Optional.ofNullable(emptyCondition.getPredefinedFieldCode()).orElse(new ArrayList<>());
            emptyCondition.setPredefinedFieldCode(predefinedFieldCode);
            if (!predefinedFieldCode.contains(fieldCode)) {
                predefinedFieldCode.add(fieldCode);
            }
        }
        if (fieldId != null) {
            List<Long> customFieldId = Optional.ofNullable(emptyCondition.getCustomFieldIds()).orElse(new ArrayList<>());
            emptyCondition.setCustomFieldIds(customFieldId);
            if (!customFieldId.contains(fieldId)) {
                customFieldId.add(fieldId);
            }
        }
    }
}
