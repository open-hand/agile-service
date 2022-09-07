package io.choerodon.agile.infra.upgrade;

import java.util.*;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.BeanPropertyRowMapper;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import io.choerodon.agile.api.vo.ProjectVO;
import io.choerodon.agile.infra.dto.PersonalFilterDTO;
import io.choerodon.agile.infra.enums.ProjectCategory;
import io.choerodon.agile.infra.feign.operator.RemoteIamOperator;
import io.choerodon.agile.infra.feign.vo.ProjectCategoryDTO;
import io.choerodon.agile.infra.mapper.PersonalFilterMapper;

import org.hzero.core.base.BaseConstants;

/**
 * 升级数据修复专用Service
 * @author gaokuo.dai@zknow.com 2022-09-06
 */
@Service
public class UpgradeDataFixService {

    @Autowired
    private JdbcTemplate jdbcTemplate;
    @Autowired
    private RemoteIamOperator iamOperator;
    @Autowired
    private PersonalFilterMapper personalFilterMapper;

    /**
     * 纯瀑布
     */
    private String PURE_WF = "PURE_WF";
    /**
     * 纯项目群
     */
    private String PURE_PROGRAM = "PURE_PROGRAM";
    /**
     * 瀑布敏捷
     */
    private String AGILE_WF = "AGILE_WF";
    /**
     * 项目群敏捷
     */
    private String AGILE_PROGRAM = "AGILE_PROGRAM";

    /**
     * 2.2版本-修复个人筛选<br/>
     * 详见<a href="https://choerodon.com.cn/#/agile/work-list/issue?type=project&id=261445508798373888&name=%E7%87%95%E5%8D%83%E6%95%8F%E6%8D%B7%E5%8D%8F%E4%BD%9C%E7%AE%A1%E7%90%86&category=AGILE&organizationId=1128&paramIssueId=346950576628551680&paramName=yq-pm-3211">路线图、项目计划保存个人筛选使用不同适用范围code-历史数据修复</a>
     */
    @Transactional(rollbackFor = Exception.class)
    public void fixAgilePersonalFilter2_2() {
        final List<Long> projectIdsForFix = this.jdbcTemplate.queryForList(
                "SELECT DISTINCT project_id FROM agile_personal_filter WHERE filter_type_code = 'agile_issue'",
                Long.class
        );
        if (CollectionUtils.isEmpty(projectIdsForFix)) {
            return;
        }
        final List<ProjectVO> projectsForFix = this.iamOperator.queryProjectByIds(new HashSet<>(projectIdsForFix));
        if (CollectionUtils.isEmpty(projectsForFix)) {
            return;
        }
        final Map<String, List<ProjectVO>> projectTypeToProjectMap = projectsForFix.stream().collect(Collectors.groupingBy(this::getProjectType));
        // 修复纯瀑布
        final List<ProjectVO> pureWfProjects = projectTypeToProjectMap.get(PURE_WF);
        if (CollectionUtils.isNotEmpty(pureWfProjects)) {
            final List<Long> projectIds = pureWfProjects.stream().map(ProjectVO::getId).collect(Collectors.toList());
            this.jdbcTemplate.update(
                    "UPDATE agile_personal_filter \n" +
                            "SET filter_type_code = 'waterfall_issue' \n" +
                            "WHERE\n" +
                            "\tfilter_id IN (\n" +
                            "\tSELECT\n" +
                            "\t\tfilter_id \n" +
                            "\tFROM\n" +
                            "\t\t(\n" +
                            "\t\tSELECT\n" +
                            "\t\t\tfilter_id \n" +
                            "\t\tFROM\n" +
                            "\t\t\tagile_personal_filter A \n" +
                            "\t\tWHERE\n" +
                            "\t\t\tA.project_id IN (" + projectIds.stream().map(id -> BaseConstants.Symbol.QUESTION).collect(Collectors.joining(BaseConstants.Symbol.COMMA)) + ")\n" +
                            "\t\t\tAND A.filter_type_code = 'agile_issue' \n" +
                            "\t\t\tAND NOT EXISTS (\n" +
                            "\t\t\tSELECT\n" +
                            "\t\t\t\t1 \n" +
                            "\t\t\tFROM\n" +
                            "\t\t\t\tagile_personal_filter B \n" +
                            "\t\t\tWHERE\n" +
                            "\t\t\t\tB.project_id = A.project_id \n" +
                            "\t\t\t\tAND B.user_id = A.user_id \n" +
                            "\t\t\t\tAND B.NAME = A.NAME \n" +
                            "\t\t\t\tAND B.filter_type_code = 'waterfall_issue' \n" +
                            "\t\t\t)) temp \n" +
                            "\t)",
                    (ps -> {
                        for (int i = 1; i <= projectIds.size(); i++) {
                            ps.setLong(i, projectIds.get(i));
                        }
                    })
            );
        }
        // 修复纯项目群
        final List<ProjectVO> pureProgramProjects = projectTypeToProjectMap.get(PURE_PROGRAM);
        if (CollectionUtils.isNotEmpty(pureProgramProjects)) {
            final List<Long> projectIds = pureProgramProjects.stream().map(ProjectVO::getId).collect(Collectors.toList());
            this.jdbcTemplate.update(
                    "UPDATE agile_personal_filter \n" +
                            "SET filter_type_code = 'feature_issue' \n" +
                            "WHERE\n" +
                            "\tfilter_id IN (\n" +
                            "\tSELECT\n" +
                            "\t\tfilter_id \n" +
                            "\tFROM\n" +
                            "\t\t(\n" +
                            "\t\tSELECT\n" +
                            "\t\t\tfilter_id \n" +
                            "\t\tFROM\n" +
                            "\t\t\tagile_personal_filter A \n" +
                            "\t\tWHERE\n" +
                            "\t\t\tA.project_id IN (" + projectIds.stream().map(id -> BaseConstants.Symbol.QUESTION).collect(Collectors.joining(BaseConstants.Symbol.COMMA)) + ")\n" +
                            "\t\t\tAND A.filter_type_code = 'agile_issue' \n" +
                            "\t\t\tAND NOT EXISTS (\n" +
                            "\t\t\tSELECT\n" +
                            "\t\t\t\t1 \n" +
                            "\t\t\tFROM\n" +
                            "\t\t\t\tagile_personal_filter B \n" +
                            "\t\t\tWHERE\n" +
                            "\t\t\t\tB.project_id = A.project_id \n" +
                            "\t\t\t\tAND B.user_id = A.user_id \n" +
                            "\t\t\t\tAND B.NAME = A.NAME \n" +
                            "\t\t\t\tAND B.filter_type_code = 'feature_issue' \n" +
                            "\t\t\t)) temp \n" +
                            "\t)",
                    (ps -> {
                        for (int i = 1; i <= projectIds.size(); i++) {
                            ps.setLong(i, projectIds.get(i));
                        }
                    })
            );
        }
        // 修复瀑布敏捷
        final List<ProjectVO> agileWfProjects = projectTypeToProjectMap.get(AGILE_WF);
        if (CollectionUtils.isNotEmpty(agileWfProjects)) {
            final List<Long> projectIds = agileWfProjects.stream().map(ProjectVO::getId).collect(Collectors.toList());
            final List<PersonalFilterDTO> filtersForInsert = this.jdbcTemplate.query(
                    "SELECT\n" +
                            "\tA.project_id AS projectId,\n" +
                            "\tA.NAME AS NAME,\n" +
                            "\tA.user_id AS userId,\n" +
                            "\tA.filter_json AS filterJson,\n" +
                            "\tA.is_default AS isDefault,\n" +
                            "\tA.organization_id AS organizationId,\n" +
                            "\t'waterfall_issue' AS filter_type_code \n" +
                            "FROM\n" +
                            "\tagile_personal_filter A \n" +
                            "WHERE\n" +
                            "\tA.project_id IN (" + projectIds.stream().map(String::valueOf).collect(Collectors.joining(BaseConstants.Symbol.COMMA)) + ") \n" +
                            "\tAND A.filter_type_code = 'agile_issue' \n" +
                            "\tAND NOT EXISTS (\n" +
                            "\tSELECT\n" +
                            "\t\t1 \n" +
                            "\tFROM\n" +
                            "\t\tagile_personal_filter B \n" +
                            "\tWHERE\n" +
                            "\t\tB.project_id = A.project_id \n" +
                            "\t\tAND B.user_id = A.user_id \n" +
                            "\t\tAND B.NAME = A.NAME \n" +
                            "\tAND B.filter_type_code = 'waterfall_issue' \n" +
                            "\t)",
                    BeanPropertyRowMapper.newInstance(PersonalFilterDTO.class)
            );
            if (CollectionUtils.isNotEmpty(filtersForInsert)) {
                this.personalFilterMapper.insertList(filtersForInsert);
            }
        }
        // 修复项目群敏捷
        final List<ProjectVO> agileProgramProjects = projectTypeToProjectMap.get(AGILE_PROGRAM);
        if (CollectionUtils.isNotEmpty(agileProgramProjects)) {
            final List<Long> projectIds = agileProgramProjects.stream().map(ProjectVO::getId).collect(Collectors.toList());
            final List<PersonalFilterDTO> filtersForInsert = this.jdbcTemplate.query(
                    "SELECT\n" +
                            "\tA.project_id AS projectId,\n" +
                            "\tA.NAME AS NAME,\n" +
                            "\tA.user_id AS userId,\n" +
                            "\tA.filter_json AS filterJson,\n" +
                            "\tA.is_default AS isDefault,\n" +
                            "\tA.organization_id AS organizationId,\n" +
                            "\t'feature_issue' AS filter_type_code \n" +
                            "FROM\n" +
                            "\tagile_personal_filter A \n" +
                            "WHERE\n" +
                            "\tA.project_id IN (" + projectIds.stream().map(String::valueOf).collect(Collectors.joining(BaseConstants.Symbol.COMMA)) + ") \n" +
                            "\tAND A.filter_type_code = 'agile_issue' \n" +
                            "\tAND NOT EXISTS (\n" +
                            "\tSELECT\n" +
                            "\t\t1 \n" +
                            "\tFROM\n" +
                            "\t\tagile_personal_filter B \n" +
                            "\tWHERE\n" +
                            "\t\tB.project_id = A.project_id \n" +
                            "\t\tAND B.user_id = A.user_id \n" +
                            "\t\tAND B.NAME = A.NAME \n" +
                            "\tAND B.filter_type_code = 'feature_issue' \n" +
                            "\t)",
                    BeanPropertyRowMapper.newInstance(PersonalFilterDTO.class)
            );
            if (CollectionUtils.isNotEmpty(filtersForInsert)) {
                this.personalFilterMapper.insertList(filtersForInsert);

            }
        }
    }

    /**
     * 鉴定项目类型
     * @param project 项目信息
     * @return 项目类型
     */
    private String getProjectType(ProjectVO project) {
        if(project == null) {
            return StringUtils.EMPTY;
        }
        final List<String> categoryCodes = Optional.ofNullable(project.getCategories())
                .orElse(Collections.emptyList())
                .stream()
                .map(ProjectCategoryDTO::getCode)
                .collect(Collectors.toList());
        if(categoryCodes.contains(ProjectCategory.MODULE_AGILE) && categoryCodes.contains(ProjectCategory.MODULE_PROGRAM)) {
            // 带敏捷的项目群: N_AGILE + N_PROGRAM
            return AGILE_PROGRAM;
        } else if(categoryCodes.contains(ProjectCategory.MODULE_WATERFALL_AGILE) && categoryCodes.contains(ProjectCategory.MODULE_WATERFALL)) {
            // 大瀑布小敏捷: N_WATERFALL_AGILE + N_WATERFALL
            return AGILE_WF;
        } else if(categoryCodes.contains(ProjectCategory.MODULE_PROGRAM)) {
            // 纯项目群: N_PROGRAM
            return PURE_PROGRAM;
        } else if(categoryCodes.contains(ProjectCategory.MODULE_WATERFALL)) {
            // 纯瀑布: N_WATERFALL
            return PURE_WF;
        } else {
            return StringUtils.EMPTY;
        }
    }

}
