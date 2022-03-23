package io.choerodon.agile.infra.enums;

import io.choerodon.agile.api.vo.ProjectVO;
import io.choerodon.agile.infra.feign.vo.ProjectCategoryDTO;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.core.exception.CommonException;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * 项目类别
 *
 * @author shinan.chen
 * @date 2019/03/13
 */
public class ProjectCategory {
    private ProjectCategory() {
    }

    /**
     * 敏捷项目
     */
    public static final String AGILE = "AGILE";
    /**
     * 普通项目组
     */
    public static final String PROGRAM = "PROGRAM";
    /**
     * 普通应用项目
     */
    public static final String GENERAL = "GENERAL";
    /**
     * 运维项目
     */
    public static final String OPERATIONS = "OPERATIONS";
    /**
     * 项目群子项目
     */
    public static final String PROGRAM_PROJECT = "PROGRAM_PROJECT";


    /**
     * 敏捷管理模块
     */
    public static final String MODULE_AGILE = "N_AGILE";

    /**
     * 敏捷项目群模块
     */
    public static final String MODULE_PROGRAM = "N_PROGRAM";
    /**
     * 敏捷项目群子项目模块
     */
    public static final String MODULE_PROGRAM_PROJECT = "N_PROGRAM_PROJECT";
    /**
     * 需求管理模块
     */
    public static final String MODULE_BACKLOG = "N_REQUIREMENT";
    /**
     * devops流程模块
     */
    public static final String MODULE_DEVOPS = "N_DEVOPS";

    /**
     * 瀑布模块
     */
    public static final String MODULE_WATERFALL = "N_WATERFALL";

    /**
     * 开启冲刺的瀑布模块
     */
    public static final String MODULE_WATERFALL_AGILE = "N_WATERFALL_AGILE";

    private static final String ERROR_CATEGORIES_IS_NULL = "error.categories.is.null";

    public static String getApplyType(String category){
        if (PROGRAM.equals(category)) {
            return SchemeApplyType.PROGRAM;
        } else if (AGILE.equals(category) || GENERAL.equals(category) || OPERATIONS.equals(category)) {
            return SchemeApplyType.AGILE;
        } else {
            return null;
        }
    }
    public static Boolean checkContainProjectCategory(List<ProjectCategoryDTO> categories, String category){
        if (CollectionUtils.isEmpty(categories)) {
            categories = new ArrayList<>();
        }
        Set<String> codes = categories.stream().map(ProjectCategoryDTO::getCode).collect(Collectors.toSet());
        return codes.contains(category);
    }

    public static List<String> getProjectApplyType(Long projectId) {
        ProjectVO projectVO = ConvertUtil.queryProject(projectId);
        if (CollectionUtils.isEmpty(projectVO.getCategories())) {
            throw new CommonException(ERROR_CATEGORIES_IS_NULL);
        }
        List<String> applyTypes = new ArrayList<>();
        Set<String> codes = projectVO.getCategories().stream().map(ProjectCategoryDTO::getCode).collect(Collectors.toSet());
        if (codes.contains(ProjectCategory.MODULE_PROGRAM)) {
            applyTypes.add(SchemeApplyType.PROGRAM);
        }
        if (codes.contains(ProjectCategory.MODULE_AGILE)
                || codes.contains(ProjectCategory.MODULE_WATERFALL_AGILE)) {
            applyTypes.add(SchemeApplyType.AGILE);
        }
        if (codes.contains(ProjectCategory.MODULE_WATERFALL)) {
            applyTypes.add(SchemeApplyType.WATERFALL);
        }
        return applyTypes;
    }

    public static Boolean consumeProjectCreatEvent(Set<String> codes) {
        if (ObjectUtils.isEmpty(codes)) {
            return false;
        }
        boolean containsBacklog = codes.contains(MODULE_BACKLOG);
        boolean containsWaterfall = codes.contains(MODULE_WATERFALL);
        boolean containsAgile = codes.contains(MODULE_AGILE);
        boolean containsProgram = codes.contains(MODULE_PROGRAM);
        if (containsWaterfall && (containsAgile || containsProgram)) {
            throw new CommonException("can not create N_WATERFALL with N_AGILE or N_PROGRAM");
        }
        if (containsBacklog
                && !(containsAgile || containsProgram || containsWaterfall)) {
            throw new CommonException("can not create N_REQUIREMENT without N_AGILE or N_PROGRAM or N_WATERFALL");
        }
        return containsAgile
                || containsProgram
                || containsWaterfall;
    }

    public static String getProjectIssueTypeList(Long projectId) {
        ProjectVO projectVO = ConvertUtil.queryProject(projectId);
        if (CollectionUtils.isEmpty(projectVO.getCategories())) {
            throw new CommonException(ERROR_CATEGORIES_IS_NULL);
        }
        String issueTypeList = "agileIssueType";
        Set<String> codes = projectVO.getCategories().stream().map(ProjectCategoryDTO::getCode).collect(Collectors.toSet());
        if (codes.contains(ProjectCategory.MODULE_WATERFALL_AGILE)) {
            return "waterfallAgileIssueType";
        }
        if (codes.contains(ProjectCategory.MODULE_WATERFALL)) {
            return "waterfallIssueType";
        }
        return issueTypeList;
    }

    public static boolean isWaterfallProject(Long projectId) {
        ProjectVO projectVO = ConvertUtil.queryProject(projectId);
        List<ProjectCategoryDTO> categories = projectVO.getCategories();
        if (CollectionUtils.isEmpty(categories)) {
            throw new CommonException(ERROR_CATEGORIES_IS_NULL);
        }
        Set<String> codes = categories.stream().map(ProjectCategoryDTO::getCode).collect(Collectors.toSet());
        return codes.contains(MODULE_WATERFALL) || codes.contains(MODULE_WATERFALL_AGILE);
    }

    public static List<String> getProjectCategoryCodes(ProjectVO projectVO) {
        if (Objects.isNull(projectVO)) {
            throw new CommonException("error.project.not.existed");
        }
        List<ProjectCategoryDTO> categories = projectVO.getCategories();
        if (CollectionUtils.isEmpty(categories)) {
            return new ArrayList<>();
        }
        return categories.stream().map(ProjectCategoryDTO::getCode).collect(Collectors.toList());
    }

    public static boolean containsAgile(List<String> categories) {
        if (ObjectUtils.isEmpty(categories)) {
            return false;
        }
        return categories.contains(MODULE_AGILE) || categories.contains(MODULE_WATERFALL_AGILE);
    }
}
