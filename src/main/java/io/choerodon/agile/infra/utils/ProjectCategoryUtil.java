package io.choerodon.agile.infra.utils;

import io.choerodon.agile.infra.enums.ProjectCategory;
import io.choerodon.agile.infra.feign.vo.ProjectCategoryDTO;
import io.choerodon.core.exception.CommonException;
import org.springframework.util.ObjectUtils;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * @author superlee
 * @since 2021-01-18
 */
public class ProjectCategoryUtil {

    public static Set<String> getCategoryCodeAndValidate(List<ProjectCategoryDTO> categories) {
        if (ObjectUtils.isEmpty(categories)) {
            return new HashSet<>();
        }
        Set<String> codes = categories.stream().map(ProjectCategoryDTO::getCode).collect(Collectors.toSet());
        if (codes.contains(ProjectCategory.MODULE_AGILE)
                && codes.contains(ProjectCategory.MODULE_PROGRAM)) {
            throw new CommonException("error.illegal.project.category.agile.and.program");
        }
        if (!codes.contains(ProjectCategory.MODULE_AGILE)
                && !codes.contains(ProjectCategory.MODULE_PROGRAM)) {
            throw new CommonException("error.illegal.project.category.not.agile.and.program");
        }
        return codes;
    }
}
