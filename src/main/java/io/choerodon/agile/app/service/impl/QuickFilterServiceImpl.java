package io.choerodon.agile.app.service.impl;

import java.io.IOException;
import java.util.*;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import io.choerodon.agile.api.vo.QuickFilterSearchVO;
import io.choerodon.agile.api.vo.QuickFilterSequenceVO;
import io.choerodon.agile.api.vo.QuickFilterVO;
import io.choerodon.agile.api.vo.QuickFilterValueVO;
import io.choerodon.agile.app.service.AgilePluginService;
import io.choerodon.agile.app.service.ObjectSchemeFieldService;
import io.choerodon.agile.app.service.QuickFilterFieldService;
import io.choerodon.agile.app.service.QuickFilterService;
import io.choerodon.agile.infra.dto.BoardQuickFilterRelDTO;
import io.choerodon.agile.infra.dto.ObjectSchemeFieldDTO;
import io.choerodon.agile.infra.dto.QuickFilterDTO;
import io.choerodon.agile.infra.dto.QuickFilterFieldDTO;
import io.choerodon.agile.infra.enums.CustomFieldType;
import io.choerodon.agile.infra.mapper.BoardQuickFilterRelMapper;
import io.choerodon.agile.infra.mapper.QuickFilterFieldMapper;
import io.choerodon.agile.infra.mapper.QuickFilterMapper;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.agile.infra.utils.EncryptionUtils;
import io.choerodon.agile.infra.utils.PageUtil;
import io.choerodon.agile.infra.utils.ProjectUtil;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.CommonException;
import io.choerodon.mybatis.pagehelper.PageHelper;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.hzero.starter.keyencrypt.core.EncryptProperties;
import org.hzero.starter.keyencrypt.core.EncryptionService;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

/**
 * Created by HuangFuqiang@choerodon.io on 2018/6/13.
 * Email: fuqianghuang01@gmail.com
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class QuickFilterServiceImpl implements QuickFilterService {

    private static final Logger LOGGER = LoggerFactory.getLogger(QuickFilterServiceImpl.class);

    protected static final String NOT_IN = "not in";
    protected static final String IS_NOT = "is not";
    protected static final String NULL_STR = "null";
    protected static final String IS = "is";

    private static final List<String> EQUALS_OR_IN_LIST = Arrays.asList("=", "in");
    private static final List<String> NOT_EQUALS_OR_NOT_IN_LIST = Arrays.asList("!=", "not in");
    private static final String ACTUAL_START_TIME = "actual_start_time";
    private static final String ACTUAL_END_TIME = "actual_end_time";
    private static final String ESTIMATED_START_TIME = "estimated_start_time";
    private static final String ESTIMATED_END_TIME = "estimated_end_time";
    private static final String CREATION_DATE = "creation_date";
    private static final String LAST_UPDATE_DATE = "last_update_date";

    @Autowired
    private QuickFilterMapper quickFilterMapper;

    @Autowired
    protected QuickFilterFieldMapper quickFilterFieldMapper;

    @Autowired
    private ObjectSchemeFieldService objectSchemeFieldService;

    @Autowired
    private ProjectUtil projectUtil;

    private static final String NOT_FOUND = "error.QuickFilter.notFound";
    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private ObjectMapper objectMapper;
    @Autowired
    private QuickFilterFieldService quickFilterFieldService;
    @Autowired(required = false)
    private AgilePluginService agilePluginService;
    @Autowired
    private BoardQuickFilterRelMapper boardQuickFilterRelMapper;

    private EncryptionService encryptionService = new EncryptionService(new EncryptProperties());

    protected void dealCaseComponent(String field, String value, String operation, StringBuilder sqlQuery) {
        if (NULL_STR.equals(value)) {
            if (IS.equals(operation)) {
                sqlQuery.append(" issue_id not in ( select issue_id from agile_component_issue_rel )  ");
            } else if (IS_NOT.equals(operation)) {
                sqlQuery.append(" issue_id in ( select issue_id from agile_component_issue_rel )  ");
            }
        } else {
            if (NOT_IN.equals(operation)) {
                sqlQuery.append(" issue_id not in ( select issue_id from agile_component_issue_rel where component_id in " + inSql(operation,value) + " ) ");
            } else {
                sqlQuery.append(" issue_id in ( select issue_id from agile_component_issue_rel where " + field + " " + operation + " " + inSql(operation,value) + " ) ");
            }
        }
    }

    private String inSql(String operation,String value){
        String sql = null;
        if (NULL_STR.equals(value)) {
            sql = value;
        }
        else {
            if(NOT_IN.equals(operation) || "in".equals(operation)){
                String[] split = EncryptionUtils.subString(value);
                List<Long> list = EncryptionUtils.decryptList(Arrays.asList(split), EncryptionUtils.BLANK_KEY, null);
                sql = "(" +StringUtils.join(list,",")+ ")";
            }
            else {
                sql = EncryptionUtils.decrypt(value,EncryptionUtils.BLANK_KEY).toString();
            }
        }
        return sql;
    }

    private void dealFixVersion(QuickFilterValueVO quickFilterValueVO, String field, String value, String operation, StringBuilder sqlQuery) {
        if (NULL_STR.equals(value)) {
            if (IS.equals(operation)) {
                sqlQuery.append(" issue_id not in ( select issue_id from agile_version_issue_rel where relation_type = 'fix' ) ");
            } else if (IS_NOT.equals(operation)) {
                sqlQuery.append(" issue_id in ( select issue_id from agile_version_issue_rel where relation_type = 'fix' ) ");
            }
        } else {
            if (NOT_IN.equals(operation)) {
                sqlQuery.append(" issue_id not in ( select issue_id from agile_version_issue_rel where version_id in " + inSql(operation,value) + " and relation_type = 'fix' ) ");
            } else {
                sqlQuery.append(" issue_id in ( select issue_id from agile_version_issue_rel where " + field + " " + quickFilterValueVO.getOperation() + " " + inSql(operation,value) + " and relation_type = 'fix' ) ");
            }
        }
    }

    private void dealInfluenceVersion(QuickFilterValueVO quickFilterValueVO, String field, String value, String operation, StringBuilder sqlQuery) {
        if (NULL_STR.equals(value)) {
            if (IS.equals(operation)) {
                sqlQuery.append(" issue_id not in ( select issue_id from agile_version_issue_rel where relation_type = 'influence' ) ");
            } else if (IS_NOT.equals(operation)) {
                sqlQuery.append(" issue_id in ( select issue_id from agile_version_issue_rel where relation_type = 'influence' ) ");
            }
        } else {
            if (NOT_IN.equals(operation)) {
                sqlQuery.append(" issue_id not in ( select issue_id from agile_version_issue_rel where version_id in " + inSql(operation,value) + " and relation_type = 'influence' ) ");
            } else {
                sqlQuery.append(" issue_id in ( select issue_id from agile_version_issue_rel where " + field + " " + quickFilterValueVO.getOperation() + " " + inSql(operation,value) + " and relation_type = 'influence' ) ");
            }
        }
    }

    protected void dealCaseVersion(QuickFilterValueVO quickFilterValueVO, String field, String value, String operation, StringBuilder sqlQuery) {
        if ("fix_version".equals(quickFilterValueVO.getFieldCode())) {
            dealFixVersion(quickFilterValueVO, field, value, operation, sqlQuery);
        } else if ("influence_version".equals(quickFilterValueVO.getFieldCode())) {
            dealInfluenceVersion(quickFilterValueVO, field, value, operation, sqlQuery);
        }
    }

    protected void dealCaseLabel(String field, String value, String operation, StringBuilder sqlQuery) {
        if (NULL_STR.equals(value)) {
            if (IS.equals(operation)) {
                sqlQuery.append(" issue_id not in ( select issue_id from agile_label_issue_rel ) ");
            } else if (IS_NOT.equals(operation)) {
                sqlQuery.append(" issue_id in ( select issue_id from agile_label_issue_rel ) ");
            }
        } else {
            if (NOT_IN.equals(operation)) {
                sqlQuery.append(" issue_id not in ( select issue_id from agile_label_issue_rel where label_id in " + inSql(operation,value) + " ) ");
            } else {
                sqlQuery.append(" issue_id in ( select issue_id from agile_label_issue_rel where " + field + " " + operation + " " + inSql(operation,value) + " ) ");
            }
        }
    }

    protected void dealCaseSprint(String field, String value, String operation, StringBuilder sqlQuery) {
        if (NULL_STR.equals(value)) {
            if (IS.equals(operation)) {
                sqlQuery.append(" issue_id not in ( select issue_id from agile_issue_sprint_rel ) ");
            } else if (IS_NOT.equals(operation)) {
                sqlQuery.append(" issue_id in ( select issue_id from agile_issue_sprint_rel ) ");
            }
        } else {
            if (NOT_IN.equals(operation)) {
                sqlQuery.append(" issue_id not in ( select issue_id from agile_issue_sprint_rel where sprint_id in " + inSql(operation,value) + " ) ");
            } else {
                sqlQuery.append(" issue_id in ( select issue_id from agile_issue_sprint_rel where " + field + " " + operation + " " + inSql(operation,value) + " ) ");
            }
        }
    }

    private String getSqlQuery(QuickFilterVO quickFilterVO, Long projectId) {
        Long organizationId = projectUtil.getOrganizationId(projectId);
        List<QuickFilterValueVO> quickFilterValueVOList = quickFilterVO.getQuickFilterValueVOList();
        List<String> relationOperations = quickFilterVO.getRelationOperations();
        Boolean childIncluded = quickFilterVO.getChildIncluded();
        StringBuilder sqlQuery = new StringBuilder();
        int idx = 0;
        for (QuickFilterValueVO quickFilterValueVO : quickFilterValueVOList) {
            Boolean predefined = quickFilterValueVO.getPredefined();
            String fieldCode = quickFilterValueVO.getFieldCode();
            if (ObjectUtils.isEmpty(predefined)) {
                String errorMsg = "error." + fieldCode + ".predefined.null";
                throw new CommonException(errorMsg);
            }
            if (Boolean.TRUE.equals(predefined)) {
                appendPredefinedFieldSql(sqlQuery, quickFilterValueVO, projectId);
            } else {
                sqlQuery.append(appendCustomFieldSql(quickFilterValueVO, organizationId, projectId));
            }
            int length = relationOperations.size();
            if (idx < length && !relationOperations.get(idx).isEmpty()) {
                sqlQuery.append(relationOperations.get(idx) + " ");
                idx++;
            }
        }
        if (Boolean.FALSE.equals(childIncluded)) {
            sqlQuery.append(" and type_code != 'sub_task' ");
        }
        return sqlQuery.toString();
    }

    private String appendCustomFieldSql(QuickFilterValueVO quickFilterValueVO, Long organizationId, Long projectId) {
        String fieldCode = quickFilterValueVO.getFieldCode();
        ObjectSchemeFieldDTO objectSchemeField = objectSchemeFieldService.queryByFieldCode(organizationId, projectId, fieldCode);
        if (ObjectUtils.isEmpty(objectSchemeField)) {
            throw new CommonException("error.custom.field." + fieldCode + ".not.existed");
        }
        Long fieldId = objectSchemeField.getId();
        String value = "'null'".equals(quickFilterValueVO.getValue()) ? NULL_STR : quickFilterValueVO.getValue();
        String operation = quickFilterValueVO.getOperation();
        String customFieldType = quickFilterValueVO.getCustomFieldType();
        CustomFieldType.contains(customFieldType, true);

        String selectSql =
                " select ffv.instance_id from fd_field_value ffv where ffv.project_id = " + projectId
                        + " and ffv.field_id = " + fieldId;
        if (CustomFieldType.isOption(customFieldType)) {
            return getOptionOrNumberSql(NULL_STR.equals(value)? value : EncryptionUtils.handlerFilterEncryptList(value,false), operation, selectSql, "ffv.option_id");
        } else if (CustomFieldType.isDate(customFieldType)) {
            return getDateSql(value, operation, selectSql);
        } else if (CustomFieldType.isDateHms(customFieldType)) {
            return getDateHmsSql(value, operation, selectSql);
        } else if (CustomFieldType.isNumber(customFieldType)) {
            return getOptionOrNumberSql(value, operation, selectSql, "ffv.number_value");
        } else if (CustomFieldType.isString(customFieldType)) {
            return getStringOrTextSql(value, operation, selectSql, "ffv.string_value");
        } else  {
            //text
            return getStringOrTextSql(value, operation, selectSql, "ffv.text_value");
        }
    }

    private String getStringOrTextSql(String value, String operation, String selectSql, String column) {
        if (NULL_STR.equals(value)) {
            if (IS.equals(operation)) {
                StringBuilder build = new StringBuilder(" issue_id not in ( ");
                build.append(selectSql).append(")");
                return build.toString();
            } else if (IS_NOT.equals(operation)) {
                StringBuilder build = new StringBuilder(" issue_id in ( ");
                build.append(selectSql).append(")");
                return build.toString();
            } else {
                return "1=1";
            }
        } else {
            StringBuilder likeSql = new StringBuilder();
            likeSql
                    .append(" ( ")
                    .append(selectSql)
                    .append(" and ")
                    .append(column)
                    .append(" like concat(concat('%', '")
                    .append(value)
                    .append("'), '%')")
                    .append(" ) ");
            if ("not like".equals(operation)) {
                return "issue_id not in " + likeSql.toString();
            } else if ("like".equals(operation)) {
                return "issue_id in " + likeSql.toString();
            } else {
                StringBuilder builder = new StringBuilder(" issue_id in ");
                builder
                        .append(" ( ")
                        .append(selectSql)
                        .append(" and ")
                        .append(column)
                        .append(" ")
                        .append(operation)
                        .append(" '")
                        .append(value)
                        .append("' ) ");
                return builder.toString();

            }
        }
    }

    private String getOptionOrNumberSql(String value, String operation, String selectSql, String column) {
        if (NULL_STR.equals(value)) {
            if (IS.equals(operation)) {
                StringBuilder build = new StringBuilder(" issue_id not in ( ");
                build.append(selectSql).append(")");
                return build.toString();
            } else if (IS_NOT.equals(operation)) {
                StringBuilder build = new StringBuilder(" issue_id in ( ");
                build.append(selectSql).append(")");
                return build.toString();
            } else {
                return "1=1";
            }
        } else {
            if (NOT_IN.equals(operation)) {
                StringBuilder builder = new StringBuilder(" issue_id not in ");
                builder
                        .append(" ( ")
                        .append(selectSql)
                        .append(" and ")
                        .append(column)
                        .append(" in ")
                        .append(value)
                        .append(" ) ");
                return builder.toString();
            } else {
                StringBuilder builder = new StringBuilder(" issue_id in ");
                builder
                        .append(" ( ")
                        .append(selectSql)
                        .append(" and ")
                        .append(column)
                        .append(" ")
                        .append(operation)
                        .append(" ")
                        .append(value)
                        .append(" ) ");
                return builder.toString();
            }
        }
    }

    private String getDateHmsSql(String value, String operation, String selectSql) {
        StringBuilder build = new StringBuilder(" issue_id in ");
        build
                .append(" ( ")
                .append(selectSql)
                .append(" and time(DATE_FORMAT(ffv.date_value, '%H:%i:%s')) ")
                .append(operation)
                .append(" time('")
                .append(value)
                .append("')) ");
        return build.toString();
    }

    private String getDateSql(String value, String operation, String selectSql) {
        StringBuilder build = new StringBuilder(" issue_id in ");
        build
                .append(" ( ")
                .append(selectSql)
                .append(" and unix_timestamp(ffv.date_value) ")
                .append(operation)
                .append(" unix_timestamp('")
                .append(value)
                .append("')) ");
        return build.toString();
    }

    protected void appendPredefinedFieldSql(StringBuilder sqlQuery, QuickFilterValueVO quickFilterValueVO, Long projectId) {
        String value = "'null'".equals(quickFilterValueVO.getValue()) ? NULL_STR : quickFilterValueVO.getValue();
        String operation = quickFilterValueVO.getOperation();
        if (agilePluginService != null && "feature".equals(quickFilterValueVO.getFieldCode())) {
            agilePluginService.appendProgramFieldSql(sqlQuery, quickFilterValueVO, value, operation, projectId);
        }
        else {
            processPredefinedField(sqlQuery, quickFilterValueVO, value, operation);
        }
    }

    protected void processPredefinedField(StringBuilder sqlQuery, QuickFilterValueVO quickFilterValueVO, String value, String operation) {
        String field = quickFilterFieldMapper.selectByPrimaryKey(quickFilterValueVO.getFieldCode()).getField();
        switch (field) {
            case "component_id":
                dealCaseComponent(field, value, operation, sqlQuery);
                break;
            case "version_id":
                dealCaseVersion(quickFilterValueVO, field, value, operation, sqlQuery);
                break;
            case "label_id":
                dealCaseLabel(field, value, operation, sqlQuery);
                break;
            case "sprint_id":
                dealCaseSprint(field, value, operation, sqlQuery);
                break;
            case CREATION_DATE:
            case LAST_UPDATE_DATE:
            case ESTIMATED_START_TIME:
            case ESTIMATED_END_TIME:
            case ACTUAL_START_TIME:
            case ACTUAL_END_TIME:
                sqlQuery.append(" unix_timestamp(" + field + ")" + " " + quickFilterValueVO.getOperation() + " " + "unix_timestamp('" + value + "') ");
                break;
            case "participant_id":
                dealParticipant(field, value, operation, sqlQuery);
                break;
            case "assignee_id":
                dealAssigneeId(field, value, operation, sqlQuery);
                break;
            default:
                if(Arrays.asList(EncryptionUtils.FIELD_VALUE).contains(field)){
                    sqlQuery.append(" " + field + " " + quickFilterValueVO.getOperation() + " " + value + " ");
                }
                else {
                    sqlQuery.append(" " + field + " " + quickFilterValueVO.getOperation() + " " + inSql(operation,value) + " ");
                }
                break;
        }
    }

    private void dealAssigneeId(String field,
                                String value,
                                String operation,
                                StringBuilder sqlQuery) {
        String space = " ";
        StringBuilder sqlBuilder = new StringBuilder();
        String sql =
                sqlBuilder
                        .append(space)
                        .append(field)
                        .append(space)
                        .append(operation)
                        .append(space)
                        .append(inSql(operation, value))
                        .append(space)
                        .toString();
        boolean containZero;
        if (value.contains("(")) {
            List<String> valueList = Arrays.asList(EncryptionUtils.subString(value));
            containZero = valueList.contains("0");
        } else {
            containZero = "0".equals(value);
        }
        StringBuilder builder = new StringBuilder();
        if (containZero) {
            if (EQUALS_OR_IN_LIST.contains(operation.toLowerCase())) {
                builder
                        .append(" ( ")
                        .append(field)
                        .append(" is null or ")
                        .append(sql)
                        .append(" ) ");
            } else if (NOT_EQUALS_OR_NOT_IN_LIST.contains(operation.toLowerCase())) {
                builder
                        .append(" ( ")
                        .append(field)
                        .append(" is not null and ")
                        .append(sql)
                        .append(" ) ");
            } else {
                builder.append(sql);
            }
        } else {
            builder.append(sql);
        }
        sqlQuery.append(builder);
    }

    private void dealParticipant(String field, String value, String operation, StringBuilder sqlQuery) {
        if (NULL_STR.equals(value)) {
            if (IS.equals(operation)) {
                sqlQuery.append(" issue_id not in ( select issue_id from agile_issue_participant_rel ) ");
            } else if (IS_NOT.equals(operation)) {
                sqlQuery.append(" issue_id in ( select issue_id from agile_issue_participant_rel ) ");
            }
        } else {
            if (NOT_IN.equals(operation)) {
                sqlQuery.append(" issue_id not in ( select issue_id from agile_issue_participant_rel where participant_id in " + inSql(operation,value) + " ) ");
            } else {
                sqlQuery.append(" issue_id in ( select issue_id from agile_issue_participant_rel where " + field + " " + operation + " " + inSql(operation,value) + " ) ");
            }
        }
    }

    @Override
    public QuickFilterVO create(Long projectId, QuickFilterVO quickFilterVO) {
        if (!projectId.equals(quickFilterVO.getProjectId())) {
            throw new CommonException("error.projectId.notEqual");
        }
        if (Boolean.TRUE.equals(checkName(projectId, quickFilterVO.getName()))) {
            throw new CommonException("error.quickFilterName.exist");
        }
        Optional.ofNullable(quickFilterVO.getQuickFilterValueVOList())
                .orElse(Collections.emptyList()).forEach(this::decryptValueList);
        String sqlQuery = getSqlQuery(quickFilterVO, projectId);
        QuickFilterDTO quickFilterDTO = modelMapper.map(quickFilterVO, QuickFilterDTO.class);
        String description = quickFilterDTO.getDescription();
        quickFilterDTO.setDescription(handlerFilterDescription(description,false));
        quickFilterDTO.setSqlQuery(sqlQuery);
        //设置编号
        Integer sequence = quickFilterMapper.queryMaxSequenceByProject(projectId);
        quickFilterDTO.setSequence(sequence == null ? 0 : sequence + 1);
        if (quickFilterMapper.insert(quickFilterDTO) != 1) {
            throw new CommonException("error.quickFilter.insert");
        }
        return modelMapper.map(quickFilterMapper.selectByPrimaryKey(quickFilterDTO.getFilterId()), QuickFilterVO.class);
    }

    private void decryptValueList(QuickFilterValueVO filter) {
        if (Boolean.FALSE.equals(filter.getPredefined())) {
            if (CustomFieldType.isOption(filter.getCustomFieldType())) {
                filter.setValue(handlerFilterEncryptList(filter.getValue(), false));
            }
        } else {
            if (!"'null'".equals(filter.getValue())) {
                String field = Optional.ofNullable(quickFilterFieldService.selectByFieldCode(filter.getFieldCode())).map(QuickFilterFieldDTO::getField).orElse(null);
                if (!Arrays.asList(EncryptionUtils.FIELD_VALUE).contains(field)) {
                    filter.setValue(handlerFilterEncryptList(filter.getValue(), false));
                }
            }
        }
    }

    private Boolean checkNameUpdate(Long projectId, Long filterId, String quickFilterName) {
        QuickFilterDTO quickFilterDTO = quickFilterMapper.selectByPrimaryKey(filterId);
        if (quickFilterName.equals(quickFilterDTO.getName())) {
            return false;
        }
        QuickFilterDTO check = new QuickFilterDTO();
        check.setProjectId(projectId);
        check.setName(quickFilterName);
        List<QuickFilterDTO> quickFilterDTOList = quickFilterMapper.select(check);
        return quickFilterDTOList != null && !quickFilterDTOList.isEmpty();
    }

    @Override
    public QuickFilterVO update(Long projectId, Long filterId, QuickFilterVO quickFilterVO) {
        if (!projectId.equals(quickFilterVO.getProjectId())) {
            throw new CommonException("error.projectId.notEqual");
        }
        if (quickFilterVO.getName() != null && checkNameUpdate(projectId, filterId, quickFilterVO.getName())) {
            throw new CommonException("error.quickFilterName.exist");
        }
        String sqlQuery = getSqlQuery(quickFilterVO, projectId);
        quickFilterVO.setFilterId(filterId);
        QuickFilterDTO quickFilterDTO = modelMapper.map(quickFilterVO, QuickFilterDTO.class);
        quickFilterDTO.setSqlQuery(sqlQuery);
        quickFilterDTO.setDescription(handlerFilterDescription(quickFilterVO.getDescription(),false));
        return updateBySelective(quickFilterDTO);
    }

    @Override
    public void deleteById(Long projectId, Long filterId) {
        QuickFilterDTO quickFilterDTO = quickFilterMapper.selectByPrimaryKey(filterId);
        if (quickFilterDTO == null) {
            throw new CommonException("error.quickFilter.get");
        }
        if (quickFilterMapper.deleteByPrimaryKey(filterId) != 1) {
            throw new CommonException("error.quickFilter.delete");
        }
        deleteBoardQuickFilterRel(projectId, filterId);
    }

    private void deleteBoardQuickFilterRel(Long projectId, Long filterId) {
        if (ObjectUtils.isEmpty(projectId)
                || ObjectUtils.isEmpty(filterId)) {
            return;
        }
        BoardQuickFilterRelDTO rel = new BoardQuickFilterRelDTO();
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        rel.setOrganizationId(organizationId);
        rel.setProjectId(projectId);
        rel.setQuickFilterId(filterId);
        boardQuickFilterRelMapper.delete(rel);
    }

    @Override
    public QuickFilterVO queryById(Long projectId, Long filterId) {
        QuickFilterDTO quickFilterDTO = quickFilterMapper.selectByPrimaryKey(filterId);
        if (quickFilterDTO == null) {
            throw new CommonException("error.quickFilter.get");
        }
        quickFilterDTO.setDescription(handlerFilterDescription(quickFilterDTO.getDescription(),true));
        return modelMapper.map(quickFilterDTO, QuickFilterVO.class);
    }

    @Override
    public Page<QuickFilterVO> listByProjectId(Long projectId, QuickFilterSearchVO quickFilterSearchVO, PageRequest pageRequest) {
        boolean isFirstPage = (pageRequest.getPage() == 0);
        List<Long> topIds = quickFilterSearchVO.getQuickFilterIds();
        List<QuickFilterDTO> list = new ArrayList<>();
        if (isFirstPage && !ObjectUtils.isEmpty(topIds)) {
            QuickFilterSearchVO search = new QuickFilterSearchVO();
            search.setQuickFilterIds(topIds);
            list.addAll(quickFilterMapper.queryFiltersByProjectId(projectId, search));
        }
        quickFilterSearchVO.setQuickFilterIds(null);
        quickFilterSearchVO.setIgnoredQuickFilterIds(topIds);
        Page<QuickFilterDTO> page =
                PageHelper.doPageAndSort(pageRequest, () -> quickFilterMapper.queryFiltersByProjectId(projectId, quickFilterSearchVO));
        list.addAll(page.getContent());
        list.forEach(v -> v.setDescription(handlerFilterDescription(v.getDescription(), true)));
        if (list.isEmpty()) {
            return PageUtil.emptyPage(pageRequest.getPage(), pageRequest.getSize());
        }
        List<QuickFilterVO> result = modelMapper.map(list, new TypeToken<List<QuickFilterVO>>() {
        }.getType());
        return PageUtil.buildPageInfoWithPageInfoList(page, result);
    }

    @Override
    public QuickFilterVO dragFilter(Long projectId, QuickFilterSequenceVO quickFilterSequenceVO) {
        if (quickFilterSequenceVO.getAfterSequence() == null && quickFilterSequenceVO.getBeforeSequence() == null) {
            throw new CommonException("error.dragFilter.noSequence");
        }
        QuickFilterDTO quickFilterDTO = modelMapper.map(quickFilterMapper.selectByPrimaryKey(quickFilterSequenceVO.getFilterId()), QuickFilterDTO.class);
        if (quickFilterDTO == null) {
            throw new CommonException(NOT_FOUND);
        } else {
            if (quickFilterSequenceVO.getAfterSequence() == null) {
                Integer maxSequence = quickFilterMapper.queryMaxAfterSequence(quickFilterSequenceVO.getBeforeSequence(), projectId);
                quickFilterSequenceVO.setAfterSequence(maxSequence);
            } else if (quickFilterSequenceVO.getBeforeSequence() == null) {
                Integer minSequence = quickFilterMapper.queryMinBeforeSequence(quickFilterSequenceVO.getAfterSequence(), projectId);
                quickFilterSequenceVO.setBeforeSequence(minSequence);
            }
            handleSequence(quickFilterSequenceVO, projectId, quickFilterDTO);
        }
        return modelMapper.map(quickFilterMapper.selectByPrimaryKey(quickFilterSequenceVO.getFilterId()), QuickFilterVO.class);

    }

    private void handleSequence(QuickFilterSequenceVO quickFilterSequenceVO, Long projectId, QuickFilterDTO quickFilterDTO) {
        if (quickFilterSequenceVO.getBeforeSequence() == null) {
            quickFilterDTO.setSequence(quickFilterSequenceVO.getAfterSequence() + 1);
            updateBySelective(quickFilterDTO);
        } else if (quickFilterSequenceVO.getAfterSequence() == null) {
            if (quickFilterDTO.getSequence() > quickFilterSequenceVO.getBeforeSequence()) {
                Integer add = quickFilterDTO.getSequence() - quickFilterSequenceVO.getBeforeSequence();
                if (add > 0) {
                    quickFilterDTO.setSequence(quickFilterSequenceVO.getBeforeSequence() - 1);
                    updateBySelective(quickFilterDTO);
                } else {
                    quickFilterMapper.batchUpdateSequence(quickFilterSequenceVO.getBeforeSequence(), projectId,
                            quickFilterDTO.getSequence() - quickFilterSequenceVO.getBeforeSequence() + 1, quickFilterDTO.getFilterId());
                }
            }
        } else {
            Integer sequence = quickFilterSequenceVO.getAfterSequence() + 1;
            quickFilterDTO.setSequence(sequence);
            updateBySelective(quickFilterDTO);
            Integer update = sequence - quickFilterSequenceVO.getBeforeSequence();
            if (update >= 0) {
                quickFilterMapper.batchUpdateSequence(quickFilterSequenceVO.getBeforeSequence(), projectId, update + 1, quickFilterDTO.getFilterId());
            }
        }
    }

    @Override
    public Boolean checkName(Long projectId, String quickFilterName) {
        QuickFilterDTO quickFilterDTO = new QuickFilterDTO();
        quickFilterDTO.setProjectId(projectId);
        quickFilterDTO.setName(quickFilterName);
        List<QuickFilterDTO> quickFilterDTOList = quickFilterMapper.select(quickFilterDTO);
        return quickFilterDTOList != null && !quickFilterDTOList.isEmpty();
    }

    public QuickFilterVO updateBySelective(QuickFilterDTO quickFilterDTO) {
        if (quickFilterMapper.updateByPrimaryKeySelective(quickFilterDTO) != 1) {
            throw new CommonException("error.quickFilter.update");
        }
        return modelMapper.map(quickFilterMapper.selectByPrimaryKey(quickFilterDTO.getFilterId()), QuickFilterVO.class);
    }

    /**
     * 处理快速筛选描述中加解密问题
     *
     * @param description
     * @param encrypt
     * @return
     */
    public  String handlerFilterDescription(String description, boolean encrypt) {
        String prefix = description.substring(0, description.lastIndexOf('+') + 1);
        StringBuilder stringBuilder = new StringBuilder(prefix);
        String oriName = description.substring(description.lastIndexOf('+') + 1);
        try {
            JsonNode jsonNode = objectMapper.readTree(oriName);
            JsonNode arr = jsonNode.get("arr");
            stringBuilder.append("{\"arr\":[");
            if (arr.isArray()) {
                Iterator<JsonNode> elements = arr.elements();
                while (elements.hasNext()) {
                    JsonNode next = elements.next();
                    ObjectNode objectNode = (ObjectNode) next;
                    decryptFilterJson(objectNode, encrypt);
                    stringBuilder.append(objectMapper.writeValueAsString(objectNode));
                    if (elements.hasNext()) {
                        stringBuilder.append(",");
                    }
                }
            }
            stringBuilder.append("]");
            JsonNode op = jsonNode.get("o");
            if (!ObjectUtils.isEmpty(op)) {
                stringBuilder.append(",\"o\":"+objectMapper.writeValueAsString(op));
            }
            stringBuilder.append("}");
        } catch (IOException e) {
            LOGGER.error("json to json node error: {}", e);
        }
        return stringBuilder.toString();
    }

    private  void decryptFilterJson(ObjectNode objectNode, boolean encrypt) {
        boolean predefined = true;
        JsonNode jsonNode = objectNode.get("predefined");
        if(!ObjectUtils.isEmpty(jsonNode)){
            predefined = jsonNode.booleanValue();
        }
        String value = objectNode.get("value").asText();
        String fieldCode = objectNode.get("fieldCode").asText();
        if (Boolean.FALSE.equals(predefined)) {
            String customFieldType = objectNode.get("customFieldType").textValue();
            if (CustomFieldType.isOption(customFieldType)) {
                objectNode.put("value", handlerFilterEncryptList(value, encrypt));
            }
        } else {
            if (!"'null'".equals(value)) {
                String field = Optional.ofNullable(quickFilterFieldService.selectByFieldCode(fieldCode)).map(QuickFilterFieldDTO::getField).orElse(null);
                if (!Arrays.asList(EncryptionUtils.FIELD_VALUE).contains(field)) {
                    objectNode.put("value", handlerFilterEncryptList(value, encrypt));
                }
            }
        }
    }

    public String handlerFilterEncryptList(String value, boolean encrypt) {
        StringBuilder build = new StringBuilder();
        if (value.contains("(")) {
            build.append("(");
            String[] split = EncryptionUtils.subString(value);
            if (!ArrayUtils.isEmpty(split)) {
                List<String> list = Arrays.asList(split);
                for (String s : list) {
                    build.append(encryptOrDecryptValue(encrypt, s));
                    if (list.indexOf(s) != (list.size() - 1)) {
                        build.append(",");
                    }
                }
            }
            build.append(")");
        } else {
            if (StringUtils.equalsAny(value, "null", "'null'")) {
                build.append(value);
            } else {
                build.append(encryptOrDecryptValue(encrypt, value));
            }
        }
        return build.toString();
    }

    private String encryptOrDecryptValue(boolean encrypt, String value) {
        if (encrypt) {
            if (!Arrays.asList(EncryptionUtils.IGNORE_VALUES).contains(value)) {
                value = encryptionService.encrypt(value, EncryptionUtils.BLANK_KEY);
            }
        } else {
            value = EncryptionUtils.decrypt(value, EncryptionUtils.BLANK_KEY) + "";
        }
        return value;
    }
}
