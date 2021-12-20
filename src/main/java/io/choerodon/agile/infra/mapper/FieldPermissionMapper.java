package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.api.vo.PermissionVO;
import io.choerodon.agile.infra.dto.FieldPermissionDTO;
import io.choerodon.mybatis.common.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Set;

/**
 * @author superlee
 * @since 2021-07-20
 */
public interface FieldPermissionMapper extends BaseMapper<FieldPermissionDTO> {

    List<FieldPermissionDTO> selectByFieldIds(@Param("projectId") Long projectId,
                                              @Param("organizationId") Long organizationId,
                                              @Param("fieldIds") Set<Long> fieldIds,
                                              @Param("issueTypeIds") Set<Long> issueTypeIds);

    void deleteByIds(@Param("fieldPermissionIds") Set<Long> fieldPermissionIds);

    Set<Long> filterHasPermissionFields(@Param("projectId") Long projectId,
                                        @Param("organizationId") Long organizationId,
                                        @Param("issueTypeIds") Set<Long> issueTypeIds,
                                        @Param("permissionVO") PermissionVO permissionVO,
                                        @Param("fieldIds") Set<Long> fieldIds);

    boolean isPermissionsConfigured(@Param("projectId") Long projectId,
                                    @Param("organizationId") Long organizationId,
                                    @Param("issueTypeIds") Set<Long> issueTypeIds);

    List<FieldPermissionDTO> selectFieldsWithPermissions(@Param("projectId") Long projectId,
                                                         @Param("organizationId") Long organizationId,
                                                         @Param("issueTypeIds") Set<Long> issueTypeIds,
                                                         @Param("permissionVO") PermissionVO permissionVO,
                                                         @Param("fieldIds") Set<Long> fieldIds);

    List<FieldPermissionDTO> selectByOptions(@Param("projectIds") List<Long> projectIds,
                                             @Param("fieldIds") Set<Long> fieldIds,
                                             @Param("issueTypeId") Long issueTypeId);

    void deleteByFieldId(@Param("organizationId") Long organizationId,
                         @Param("projectId") Long projectId,
                         @Param("fieldId") Long fieldId,
                         @Param("issueTypeId") Long issueTypeId);
}
