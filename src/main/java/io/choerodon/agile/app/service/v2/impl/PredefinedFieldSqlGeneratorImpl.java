package io.choerodon.agile.app.service.v2.impl;

import static io.choerodon.agile.infra.enums.search.SearchConstant.SqlTemplate.*;

import java.util.*;
import java.util.stream.Collectors;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.ObjectUtils;

import io.choerodon.agile.api.vo.FieldTableVO;
import io.choerodon.agile.api.vo.business.TagVO;
import io.choerodon.agile.api.vo.search.Condition;
import io.choerodon.agile.api.vo.search.Value;
import io.choerodon.agile.app.service.AgilePluginService;
import io.choerodon.agile.app.service.AgileWaterfallService;
import io.choerodon.agile.app.service.BacklogExpandService;
import io.choerodon.agile.app.service.v2.PredefinedFieldSqlGenerator;
import io.choerodon.agile.domain.entity.SqlTemplateData;
import io.choerodon.agile.infra.dto.ProjectInfoDTO;
import io.choerodon.agile.infra.enums.FieldCode;
import io.choerodon.agile.infra.enums.InstanceType;
import io.choerodon.agile.infra.enums.search.SearchConstant;
import io.choerodon.agile.infra.mapper.ProjectInfoMapper;
import io.choerodon.agile.infra.utils.SqlUtil;
import io.choerodon.core.exception.CommonException;

import org.hzero.core.base.BaseConstants;
import org.hzero.core.util.JsonUtils;
import org.hzero.core.util.Pair;

/**
 * @author superlee
 * @since 2022-11-18
 */
@Service
public class PredefinedFieldSqlGeneratorImpl implements PredefinedFieldSqlGenerator {

    @Autowired(required = false)
    private AgilePluginService agilePluginService;
    @Autowired
    private ObjectMapper objectMapper;
    @Autowired
    private ProjectInfoMapper projectInfoMapper;
    @Autowired(required = false)
    private AgileWaterfallService agileWaterfallService;
    @Autowired(required = false)
    private BacklogExpandService backlogExpandService;

    @Override
    public String parseSql(FieldTableVO fieldTable,
                           Condition condition,
                           Set<Long> projectIds,
                           List<? extends Object> values,
                           Pair<String, String> dataPair,
                           boolean isSelector) {
        String type = fieldTable.getType();
        String sql = "";
        switch (type) {
            case FieldTableVO.TYPE_BASE:
                sql = parseBaseSql(fieldTable, condition, projectIds, values, dataPair, isSelector);
                break;
            case FieldTableVO.TYPE_PROGRAM:
                if (agilePluginService != null) {
                    sql = agilePluginService.parseProgramSql(fieldTable, condition, projectIds, values, dataPair, isSelector);
                }
                break;
            case FieldTableVO.TYPE_BACKLOG:
                if (backlogExpandService != null) {
                    sql = backlogExpandService.parseBacklogSql(fieldTable, condition, projectIds, values, dataPair, isSelector);
                }
                break;
            case FieldTableVO.TYPE_WATERFALL:
                if (agileWaterfallService != null) {
                    sql = agileWaterfallService.parseWaterfallSql(fieldTable, condition, projectIds, values, dataPair, isSelector);
                }
                break;
            case FieldTableVO.TYPE_TRIGGER:
                break;
            default:
                throw new CommonException(BaseConstants.ErrorCode.DATA_INVALID);
        }
        return sql;
    }

    private String parseBaseSql(FieldTableVO fieldTable,
                                Condition condition,
                                Set<Long> projectIds,
                                List<? extends Object> values,
                                Pair<String, String> dataPair,
                                boolean isSelector) {
        if (isSelector) {
            return parseBaseSelectorSql(fieldTable, condition, projectIds, values);
        } else {
            return parseBasePairSql(fieldTable, condition, projectIds, dataPair);
        }
    }

    private String parseBasePairSql(FieldTableVO fieldTable,
                                    Condition condition,
                                    Set<Long> projectIds,
                                    Pair<String, String> dataPair) {
        StringBuilder sqlBuilder = new StringBuilder();
        String fieldCode = fieldTable.getName();
        String operation = condition.getOperation();
        switch (fieldCode) {
            case SearchConstant.Field.CONTENT:
                sqlBuilder.append(generateContentSql(operation, dataPair, projectIds));
                break;
            default:
                sqlBuilder.append(SqlUtil.appendPredefinedSql(operation, dataPair, fieldTable, projectIds, "", null, false, InstanceType.ISSUE));
                break;
        }
        return sqlBuilder.toString();
    }

    private String parseBaseSelectorSql(FieldTableVO fieldTable,
                                        Condition condition,
                                        Set<Long> projectIds,
                                        List<?> values) {
        StringBuilder sqlBuilder = new StringBuilder();
        String fieldCode = fieldTable.getName();
        String operation = condition.getOperation();
        boolean isLinkedTable = !SearchConstant.TABLE_AGILE_ISSUE.equals(fieldTable.getTable());
        String alias = SqlUtil.ALIAS_ISSUE;
        switch (fieldCode) {
            case FieldCode.TAG:
                sqlBuilder.append(generateTagSql(operation, values, fieldTable, alias, projectIds));
                break;
            case FieldCode.FIX_VERSION:
                sqlBuilder.append(SqlUtil.generateLinkedTableSql(operation, values, fieldTable, projectIds, "and relation_type = 'fix'", null, false, InstanceType.ISSUE));
                break;
            case FieldCode.INFLUENCE_VERSION:
                sqlBuilder.append(SqlUtil.generateLinkedTableSql(operation, values, fieldTable, projectIds, "and relation_type = 'influence'", null, false, InstanceType.ISSUE));
                break;
            case SearchConstant.Field.MY_STAR:
                sqlBuilder.append(SqlUtil.generateLinkedTableSql(operation, values, fieldTable, projectIds, "and type = 'issue'", SqlUtil.INSTANCE_ID, false, InstanceType.ISSUE));
                break;
            case SearchConstant.Field.MY_PARTICIPATE:
                sqlBuilder.append(generateMyParticipateSql(projectIds, operation, alias, fieldTable, values));
                break;
            case FieldCode.EPIC:
                sqlBuilder.append(generateEpicSql(operation, values, alias, fieldTable));
                break;
            case SearchConstant.Field.ISSUE_TYPE_PROJECT:
                sqlBuilder.append(generateIssueTypeProjectSql(values));
                break;
            default:
                if (!isLinkedTable) {
                    //主表字段
                    sqlBuilder.append(SqlUtil.generateSelfTableSql(operation, values, fieldTable, "", InstanceType.ISSUE));
                } else {
                    //关联表
                    sqlBuilder.append(SqlUtil.generateLinkedTableSql(operation, values, fieldTable, projectIds, "", null, false, InstanceType.ISSUE));
                }
                break;
        }
        return sqlBuilder.toString();
    }

    private String generateIssueTypeProjectSql(List<?> values) {
        StringBuilder sqlBuilder = new StringBuilder();
        List<Value.IssueTypeProject> issueTypeProjects =
                JsonUtils.fromJson(JsonUtils.toJson(values), new TypeReference<List<Value.IssueTypeProject>>() {
                });
        if (ObjectUtils.isEmpty(issueTypeProjects)) {
            return sqlBuilder.toString();
        }
        Iterator<Value.IssueTypeProject> iterator = issueTypeProjects.iterator();
        while (iterator.hasNext()) {
            Value.IssueTypeProject issueTypeProject = iterator.next();
            Long issueTypeId = issueTypeProject.getIssueTypeId();
            Long projectId = issueTypeProject.getProjectId();
            if (ObjectUtils.isEmpty(issueTypeId) || ObjectUtils.isEmpty(projectId)) {
                continue;
            }
            sqlBuilder.append(BaseConstants.Symbol.LEFT_BRACE).append("ai.issue_type_id = ")
                    .append(issueTypeId)
                    .append(" and ai.project_id ");
            if (Objects.equals(0L, projectId)) {
                //产品要求, 只有修改过名称/图标/颜色的项目客制化工作项才记入“客制化工作项”
                //see https://choerodon.com.cn/#/agile/work-list/issue?type=project&id=261445508798373888&name=%E6%95%8F%E6%8D%B7%E5%8D%8F%E4%BD%9C%E7%BB%84&category=AGILE&organizationId=1128&paramIssueId=358361939208716288&paramName=yq-pm-3598
                sqlBuilder.append("not in (select ite.project_id from fd_issue_type_extend ite where ite.issue_type_id = ")
                        .append(issueTypeId).append(
                        " and ( ite.name is not null or ite.icon is not null or ite.colour is not null ) )");
            } else {
                sqlBuilder.append(" = ").append(projectId);
            }
            sqlBuilder.append(BaseConstants.Symbol.RIGHT_BRACE);
            if (iterator.hasNext()) {
                sqlBuilder.append(" or ");
            }
        }
        if (sqlBuilder.length() > 0) {
            return new StringBuilder().append(BaseConstants.Symbol.LEFT_BRACE).append(sqlBuilder).append(BaseConstants.Symbol.RIGHT_BRACE).toString();
        } else {
            return sqlBuilder.toString();
        }
    }

    private String generateContentSql(String operation,
                                      Pair<String, String> dataPair,
                                      Set<Long> projectIds) {
        StringBuilder sqlBuilder = new StringBuilder();
        //去除searchVO.searchArgs.issueNum或searchVO.contents的项目code前缀
        List<ProjectInfoDTO> projectInfos = projectInfoMapper.selectByProjectIds(projectIds);
        if (projectInfos.isEmpty()) {
            return sqlBuilder.toString();
        }
        List<String> projectCodes = projectInfos.stream().map(ProjectInfoDTO::getProjectCode).collect(Collectors.toList());
        String content = dataPair.getFirst();
        Pair<String, String> pair = Pair.of(null, null);
        if (!ObjectUtils.isEmpty(content)) {
            String substringContent = content;
            for (String projectCode : projectCodes) {
                String prefix = SqlUtil.SINGLE_QUOT + projectCode + "-";
                if (content.startsWith(prefix)) {
                    substringContent = content.substring(prefix.length());
                    substringContent = SqlUtil.SINGLE_QUOT + substringContent;
                    break;
                }
            }
            pair = Pair.of(substringContent, null);
        }

        sqlBuilder.append(BaseConstants.Symbol.LEFT_BRACE);
        //content == summary和issueNum
        FieldTableVO summary = SearchConstant.PREDEFINED_FIELD_TABLE_MAP.get(FieldCode.SUMMARY);
        sqlBuilder.append(SqlUtil.appendPredefinedSql(operation, pair, summary, projectIds, "", null, false, InstanceType.ISSUE));
        sqlBuilder.append(" or ");
        FieldTableVO issueNum = SearchConstant.PREDEFINED_FIELD_TABLE_MAP.get(FieldCode.ISSUE_NUM);
        sqlBuilder.append(SqlUtil.appendPredefinedSql(operation, pair, issueNum, projectIds, "", null, false, InstanceType.ISSUE));

        sqlBuilder.append(BaseConstants.Symbol.RIGHT_BRACE);
        return sqlBuilder.toString();
    }

    private String generateTagSql(String operation,
                                  List<? extends Object> values,
                                  FieldTableVO fieldTable,
                                  String alias,
                                  Set<Long> projectIds) {
        StringBuilder sqlBuilder = new StringBuilder();
        List<TagVO> tags;
        try {
            String json = objectMapper.writeValueAsString(values);
            tags = objectMapper.readValue(json, new TypeReference<List<TagVO>>() {
            });
        } catch (JsonProcessingException e) {
            throw new CommonException("error.convert.object.value", e);
        }
        if (tags == null) {
            return sqlBuilder.toString();
        }
        String primaryKey = SqlUtil.PRIMARY_KEY_ISSUE;
        String mainTableFilterColumn = SqlUtil.buildMainTableFilterColumn(primaryKey, alias);
        SearchConstant.Operation opt = SearchConstant.Operation.valueOf(operation);
        Iterator<TagVO> tagIterator = tags.iterator();
        StringBuilder conditionBuilder = new StringBuilder();
        while (tagIterator.hasNext()) {
            TagVO tag = tagIterator.next();
            conditionBuilder.append(BaseConstants.Symbol.LEFT_BRACE)
                    .append("tag_project_id = ")
                    .append(tag.getProjectId())
                    .append(" and app_service_code = ")
                    .append(SqlUtil.appendSingleQuot(tag.getAppServiceCode()))
                    .append(" and tag_name = ")
                    .append(SqlUtil.appendSingleQuot(tag.getTagName()))
                    .append(BaseConstants.Symbol.RIGHT_BRACE);
            if (tagIterator.hasNext()) {
                conditionBuilder.append(" or ");
            }
        }
        String conditionSql = conditionBuilder.toString();
        String projectIdStr = StringUtils.join(projectIds, BaseConstants.Symbol.COMMA);
        String table = fieldTable.getTable();
        SqlTemplateData data =
                new SqlTemplateData()
                        .setMainTableCol(mainTableFilterColumn)
                        .setOpt(opt.getOpt())
                        .setInnerCol(primaryKey)
                        .setTable(table)
                        .setProjectIdStr(projectIdStr)
                        .setAdditionalCondition("")
                        .setProjectCol(SqlUtil.PROJECT_ID);
        switch (opt) {
            case IN:
            case NOT_IN:
                data.setAdditionalCondition(conditionSql);
                sqlBuilder.append(SearchConstant.SqlTemplate.fillInParam(data.ofContext(), TAG_IN_OR_NOT_IN));
                break;
            case IS_NULL:
            case IS_NOT_NULL:
                sqlBuilder.append(SearchConstant.SqlTemplate.fillInParam(data.ofContext(), LINKED_TABLE_IS_NULL_OR_NOT_NULL));
                break;
            default:
                break;
        }
        return sqlBuilder.toString();
    }

    private String generateMyParticipateSql(Set<Long> projectIds,
                                            String operation,
                                            String alias,
                                            FieldTableVO fieldTable,
                                            List<? extends Object> values) {
        StringBuilder sqlBuilder = new StringBuilder();
        sqlBuilder.append(BaseConstants.Symbol.LEFT_BRACE);
        String primaryKey = SqlUtil.PRIMARY_KEY_ISSUE;
        String mainTableFilterColumn = SqlUtil.buildMainTableFilterColumn(primaryKey, alias);
        String assigneeColumn = "assignee_id";
        assigneeColumn = SqlUtil.buildMainTableFilterColumn(assigneeColumn, alias);
        String valueStr = StringUtils.join(values, BaseConstants.Symbol.COMMA);
        String projectIdStr = StringUtils.join(projectIds, BaseConstants.Symbol.COMMA);
        SearchConstant.Operation opt = SearchConstant.Operation.valueOf(operation);
        SqlTemplateData data =
                new SqlTemplateData()
                        .setMainTableCol(mainTableFilterColumn)
                        .setValue(valueStr)
                        .setOpt(opt.getOpt())
                        .setInnerCol(primaryKey)
                        .setTable(fieldTable.getTable())
                        .setProjectIdStr(projectIdStr)
                        .setColumn(assigneeColumn);
        sqlBuilder.append(SearchConstant.SqlTemplate.fillInParam(data.ofContext(), MY_PARTICIPATE));
        sqlBuilder.append(BaseConstants.Symbol.RIGHT_BRACE);
        return sqlBuilder.toString();
    }


    private String generateEpicSql(String operation, List<?> values, String alias, FieldTableVO fieldTable) {
        StringBuilder sqlBuilder = new StringBuilder();
        SearchConstant.Operation opt = SearchConstant.Operation.valueOf(operation);
        String epicIdWithAlias = SqlUtil.buildMainTableFilterColumn(fieldTable.getField(), alias);
        String valueStr = StringUtils.join(values, BaseConstants.Symbol.COMMA);
        String typeCode = SqlUtil.buildMainTableFilterColumn("type_code", alias);
        String parentIssueId = SqlUtil.buildMainTableFilterColumn("parent_issue_id", alias);
        SqlTemplateData data =
                new SqlTemplateData()
                        .setEpicIdWithAlias(epicIdWithAlias)
                        .setValue(valueStr)
                        .setOpt(opt.getOpt())
                        .setTypeCode(typeCode)
                        .setParentIssueId(parentIssueId);
        switch (opt) {
            case IN:
            case NOT_IN:
                sqlBuilder.append(SearchConstant.SqlTemplate.fillInParam(data.ofContext(), EPIC_IN_OR_NOT_IN));
                break;
            case IS_NULL:
                sqlBuilder.append(SearchConstant.SqlTemplate.fillInParam(data.ofContext(), EPIC_IS_NULL));
                break;
            case IS_NOT_NULL:
                sqlBuilder.append(SearchConstant.SqlTemplate.fillInParam(data.ofContext(), EPIC_IS_NOT_NULL));
                break;
            default:
                break;
        }
        return sqlBuilder.toString();
    }


}
