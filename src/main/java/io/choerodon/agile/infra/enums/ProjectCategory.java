package io.choerodon.agile.infra.enums;

import io.choerodon.agile.infra.feign.vo.ProjectCategoryDTO;
import io.choerodon.core.exception.CommonException;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.util.List;
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
            throw new CommonException("error.categories.is.null");
        }
        Set<String> codes = categories.stream().map(ProjectCategoryDTO::getCode).collect(Collectors.toSet());
        return codes.contains(category);
    }

    public static Boolean consumeProjectCreatEvent(Set<String> codes) {
        if (ObjectUtils.isEmpty(codes)) {
            return false;
        }
        if (codes.contains(MODULE_BACKLOG)
                && !(codes.contains(MODULE_AGILE) || codes.contains(MODULE_PROGRAM))) {
            throw new CommonException("can not create N_REQUIREMENT without N_AGILE or N_PROGRAM");
        }
        return codes.contains(MODULE_AGILE)
                || codes.contains(MODULE_PROGRAM);
    }
}
