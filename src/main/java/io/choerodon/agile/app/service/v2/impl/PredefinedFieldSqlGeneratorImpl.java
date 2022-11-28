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
import io.choerodon.agile.app.service.AgilePluginService;
import io.choerodon.agile.app.service.AgileWaterfallService;
import io.choerodon.agile.app.service.v2.PredefinedFieldSqlGenerator;
import io.choerodon.agile.domain.entity.SqlTemplateData;
import io.choerodon.agile.infra.dto.ProjectInfoDTO;
import io.choerodon.agile.infra.enums.FieldCode;
import io.choerodon.agile.infra.enums.search.SearchConstant;
import io.choerodon.agile.infra.mapper.ProjectInfoMapper;
import io.choerodon.agile.infra.utils.SqlUtil;
import io.choerodon.core.exception.CommonException;

import org.hzero.core.base.BaseConstants;
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
        String alias = "ai";
        switch (fieldCode) {
            case SearchConstant.Field.CONTENT:
                sqlBuilder.append(generateContentSql(operation, dataPair, alias, projectIds));
                break;
            default:
                sqlBuilder.append(SqlUtil.appendPredefinedSql(operation, dataPair, fieldTable, alias, projectIds, "", null, false));
                break;
        }
        return sqlBuilder.toString();
    }

    private String parseBaseSelectorSql(FieldTableVO fieldTable, Condition condition, Set<Long> projectIds, List<?> values) {
        StringBuilder sqlBuilder = new StringBuilder();
        String fieldCode = fieldTable.getName();
        String operation = condition.getOperation();
        boolean isLinkedTable = !SearchConstant.TABLE_AGILE_ISSUE.equals(fieldTable.getTable());
        String alias = "ai";
        switch (fieldCode) {
            case FieldCode.TAG:
                sqlBuilder.append(generateTagSql(operation, values, fieldTable, alias, projectIds));
                break;
            case FieldCode.FIX_VERSION:
                sqlBuilder.append(SqlUtil.generateLinkedTableSql(operation, values, fieldTable, alias, projectIds, "and relation_type = 'fix'", null, false));
                break;
            case FieldCode.INFLUENCE_VERSION:
                sqlBuilder.append(SqlUtil.generateLinkedTableSql(operation, values, fieldTable, alias, projectIds, "and relation_type = 'influence'", null, false));
                break;
            case SearchConstant.Field.MY_STAR:
                sqlBuilder.append(SqlUtil.generateLinkedTableSql(operation, values, fieldTable, alias, projectIds, "and type = 'issue'", SqlUtil.INSTANCE_ID, false));
                break;
            case SearchConstant.Field.MY_PARTICIPATE:
                sqlBuilder.append(generateMyParticipateSql(projectIds, operation, alias, fieldTable, values));
                break;
            case FieldCode.EPIC:
                sqlBuilder.append(generateEpicSql(operation, values, alias, fieldTable));
                break;
            default:
                if (!isLinkedTable) {
                    //主表字段
                    sqlBuilder.append(SqlUtil.generateSelfTableSql(operation, values, alias, fieldTable, ""));
                } else {
                    //关联表
                    sqlBuilder.append(SqlUtil.generateLinkedTableSql(operation, values, fieldTable, alias, projectIds, "", null, false));
                }
                break;
        }
        return sqlBuilder.toString();
    }

    private String generateContentSql(String operation,
                                      Pair<String, String> dataPair,
                                      String alias,
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
        sqlBuilder.append(SqlUtil.appendPredefinedSql(operation, pair, summary, "ai", projectIds, "", null, false));
        sqlBuilder.append(" or ");
        FieldTableVO issueNum = SearchConstant.PREDEFINED_FIELD_TABLE_MAP.get(FieldCode.ISSUE_NUM);
        sqlBuilder.append(SqlUtil.appendPredefinedSql(operation, pair, issueNum, "ai", projectIds, "", null, false));

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
        String primaryKey = SqlUtil.DEFAULT_PRIMARY_KEY;
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
                        .setProjectCol("project_id");
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
        String primaryKey = SqlUtil.DEFAULT_PRIMARY_KEY;
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
