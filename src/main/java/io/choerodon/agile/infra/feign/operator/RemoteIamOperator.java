package io.choerodon.agile.infra.feign.operator;

import java.util.List;
import java.util.Set;

import com.fasterxml.jackson.core.type.TypeReference;
import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.infra.dto.TimeZoneWorkCalendarDTO;
import io.choerodon.agile.infra.dto.UserDTO;
import io.choerodon.agile.infra.feign.RemoteIamFeignClient;
import io.choerodon.agile.infra.feign.vo.OrganizationInfoVO;
import io.choerodon.core.domain.Page;
import org.hzero.core.util.ResponseUtils;
import org.springframework.stereotype.Component;

/**
 * Copyright (c) 2022. Hand Enterprise Solution Company. All right reserved.
 *
 * @author zongqi.hao@zknow.com
 * @since 2022/7/14
 */
@Component
public class RemoteIamOperator {

    private final RemoteIamFeignClient remoteIamFeignClient;

    public RemoteIamOperator(RemoteIamFeignClient remoteIamFeignClient) {
        this.remoteIamFeignClient = remoteIamFeignClient;
    }

    public List<ProjectVO> queryProjectByIds(Set<Long> ids) {
        return ResponseUtils.getResponse(remoteIamFeignClient.queryProjectByIds(ids),
                new TypeReference<List<ProjectVO>>() {
                });
    }

    public UserDTO query(Long organizationId, Long id) {
        return ResponseUtils.getResponse(remoteIamFeignClient.query(organizationId, id), UserDTO.class);
    }

    public List<UserDTO> listUsersByIds(Long[] ids, Boolean onlyEnabled) {
        return ResponseUtils.getResponse(remoteIamFeignClient.listUsersByIds(ids, onlyEnabled),
                new TypeReference<List<UserDTO>>() {
                });
    }

    /**
     * 按照Id查询项目
     *
     * @param id 要查询的项目ID
     * @return 查询到的项目
     */
    public ProjectVO queryProject(Long id) {
        return ResponseUtils.getResponse(remoteIamFeignClient.queryProject(id), ProjectVO.class);
    }

    /**
     * 按照Id查询项目
     *
     * @param id 要查询的项目ID
     * @return 查询到的项目
     */
    public ProjectVO queryProject(Long id, Boolean withAgileInfo) {
        return ResponseUtils.getResponse(remoteIamFeignClient.queryProject(id, withAgileInfo), ProjectVO.class);
    }

    public Page<UserDTO> listUsersByProjectId(Long id, int page, int size, String param) {
        return ResponseUtils.getResponse(remoteIamFeignClient.listUsersByProjectId(id, page, size, param),
                new TypeReference<Page<UserDTO>>() {
                });
    }


    public Page<UserDTO> agileUsers(Long projectId, int page, int size, AgileUserVO agileUserVO) {
        return ResponseUtils.getResponse(remoteIamFeignClient.agileUsers(projectId, page, size, agileUserVO),
                new TypeReference<Page<UserDTO>>() {
                });
    }

    /**
     * 根据projectId和param模糊查询loginName和realName两列
     *
     * @param id    id
     * @param param param
     * @return UserVO
     */
    public Page<UserVO> list(Long id, String param) {
        return ResponseUtils.getResponse(remoteIamFeignClient.list(id, param),
                new TypeReference<Page<UserVO>>() {
                });
    }

    public List<RoleVO> listRolesWithUserCountOnProjectLevel(Long sourceId, RoleAssignmentSearchVO roleAssignmentSearchVO) {
        return ResponseUtils.getResponse(remoteIamFeignClient.listRolesWithUserCountOnProjectLevel(sourceId, roleAssignmentSearchVO),
                new TypeReference<List<RoleVO>>() {
                });
    }

    public Page<UserVO> pagingQueryUsersByRoleIdOnProjectLevel(int page, int size,
                                                               Long roleId,
                                                               Boolean doPage,
                                                               Long sourceId,
                                                               RoleAssignmentSearchVO roleAssignmentSearchVO) {
        return ResponseUtils.getResponse(remoteIamFeignClient.pagingQueryUsersByRoleIdOnProjectLevel(page, size, roleId, doPage, sourceId, roleAssignmentSearchVO),
                new TypeReference<Page<UserVO>>() {
                });
    }

    public ProjectVO getGroupInfoByEnableProject(Long organizationId, Long projectId) {
        return ResponseUtils.getResponse(remoteIamFeignClient.getGroupInfoByEnableProject(organizationId, projectId),
                ProjectVO.class);
    }

    public List<ProjectVO> getGroupInfoByEnableProjects(Long organizationId, Set<Long> projectIds) {
        return ResponseUtils.getResponse(remoteIamFeignClient.getGroupInfoByEnableProjects(organizationId, projectIds),
                new TypeReference<List<ProjectVO>>() {
                });
    }

    public Page<UserWithRoleVO> pagingQueryUsersWithProjectLevelRoles(int page, int size,
                                                                      Long sourceId,
                                                                      RoleAssignmentSearchVO roleAssignmentSearchVO,
                                                                      boolean doPage) {
        return ResponseUtils.getResponse(remoteIamFeignClient.pagingQueryUsersWithProjectLevelRoles(page, size, sourceId, roleAssignmentSearchVO, doPage),
                new TypeReference<Page<UserWithRoleVO>>() {
                });
    }

    /**
     * 根据组织id查询所有项目
     *
     * @param organizationId
     * @return
     */
    public List<ProjectVO> listProjectsByOrgId(Long organizationId) {
        return ResponseUtils.getResponse(remoteIamFeignClient.listProjectsByOrgId(organizationId),
                new TypeReference<List<ProjectVO>>() {
                });
    }

    public Page<ProjectVO> listWithCategoryByOrganizationIds(Long organizationId,
                                                      ProjectSearchVO projectSearchVO,
                                                      Integer page,
                                                      Integer size) {
        return ResponseUtils.getResponse(remoteIamFeignClient.listWithCategoryByOrganizationIds(organizationId, projectSearchVO, page, size),
                new TypeReference<Page<ProjectVO>>() {
                });
    }


    /**
     * 根据组织id获取时区工作日历
     *
     * @param organizationId 租户id
     * @return 时区日历dto
     */
    public TimeZoneWorkCalendarDTO queryTimeZoneDetailByOrganizationId(Long organizationId) {
        return ResponseUtils.getResponse(remoteIamFeignClient.queryTimeZoneDetailByOrganizationId(organizationId), TimeZoneWorkCalendarDTO.class);
    }

    /**
     * 根据年份查询工作日历假期(包含查询年份和下一年份数据)
     *
     * @param organizationId 租户id
     * @param year           年份
     * @return 日历假期信息
     */
    public List<WorkCalendarHolidayRefVO> queryWorkCalendarHolidayRelByYear(Long organizationId, Integer year) {
        return ResponseUtils.getResponse(remoteIamFeignClient.queryWorkCalendarHolidayRelByYear(organizationId, year),
                new TypeReference<List<WorkCalendarHolidayRefVO>>() {
                });
    }

    /**
     * 根据年份查询工作日历，包含当年、去年、明年
     *
     * @param organizationId 租户id
     * @param year           年份
     * @return 日历假期信息
     */
    public List<WorkCalendarHolidayRefVO> queryByYearIncludeLastAndNext(Long organizationId, Integer year) {
        return ResponseUtils.getResponse(remoteIamFeignClient.queryByYearIncludeLastAndNext(organizationId, year),
                new TypeReference<List<WorkCalendarHolidayRefVO>>() {
                });
    }


    public List<RoleVO> getUserWithProjLevelRolesByUserId(Long projectId, Long userId) {
        return ResponseUtils.getResponse(remoteIamFeignClient.getUserWithProjLevelRolesByUserId(projectId, userId),
                new TypeReference<List<RoleVO>>() {
                });
    }

    public List<UserVO> listUsersByRealNames(Boolean onlyEnabled, Set<String> realNames) {
        return ResponseUtils.getResponse(remoteIamFeignClient.listUsersByRealNames(onlyEnabled, realNames),
                new TypeReference<List<UserVO>>() {
                });
    }

    public List<UserVO> listProjectOwnerById(Long projectId) {
        return ResponseUtils.getResponse(remoteIamFeignClient.listProjectOwnerById(projectId),
                new TypeReference<List<UserVO>>() {
                });
    }

    public List<ProjectWithUserVO> listProjectOwnerByIds(Set<Long> projectIds) {
        return ResponseUtils.getResponse(remoteIamFeignClient.listProjectOwnerByIds(projectIds),
                new TypeReference<List<ProjectWithUserVO>>() {
                });
    }

    public List<UserVO> listUsersUnderRoleByIds(Long projectId, String roleIdString) {
        return ResponseUtils.getResponse(remoteIamFeignClient.listUsersUnderRoleByIds(projectId, roleIdString),
                new TypeReference<List<UserVO>>() {
                });
    }

    public Boolean checkIsProjectOwner(Long id, Long projectId) {
        return ResponseUtils.getResponse(remoteIamFeignClient.checkIsProjectOwner(id, projectId), Boolean.class);
    }

    public List<ProjectVO> queryProjects(Long id, boolean includedDisabled) {
        return ResponseUtils.getResponse(remoteIamFeignClient.queryProjects(id, includedDisabled),
                new TypeReference<List<ProjectVO>>() {
                });
    }

    public List<ProjectVO> queryOrgProjects(Long organizationId, Long userId) {
        return ResponseUtils.getResponse(remoteIamFeignClient.queryOrgProjects(organizationId, userId),
                new TypeReference<List<ProjectVO>>() {
                });
    }

    public List<RoleVO> listProjectRoles(Long projectId, Boolean onlySelectEnable, String roleName) {
        return ResponseUtils.getResponse(remoteIamFeignClient.listProjectRoles(projectId, onlySelectEnable, roleName),
                new TypeReference<List<RoleVO>>() {
                });
    }

    public Page<RoleVO> listOrganizationRoles(Integer page, Integer size, String name, String code, String roleLevel, Long tenantId, Boolean builtIn, Boolean enabled, String params) {
        return ResponseUtils.getResponse(remoteIamFeignClient.listOrganizationRoles(page, size, name, code, roleLevel, tenantId, builtIn, enabled, params),
                new TypeReference<Page<RoleVO>>() {
                });
    }

    public Page<UserVO> queryUsersByProject(Long projectId, String param, int page, int size) {
        return ResponseUtils.getResponse(remoteIamFeignClient.queryUsersByProject(projectId, param, page, size),
                new TypeReference<Page<UserVO>>() {
                });
    }

    public Page<UserVO> queryUsersByOrganization(Long projectId, String param, int page, int size) {
        return ResponseUtils.getResponse(remoteIamFeignClient.queryUsersByOrganization(projectId, param, page, size),
                new TypeReference<Page<UserVO>>() {
                });
    }

    public Page<UserDTO> pagingUsersOnOrganizationLevel(Long organizationId, int page, int size, AgileUserVO agileUserVO) {
        return ResponseUtils.getResponse(remoteIamFeignClient.pagingUsersOnOrganizationLevel(organizationId, page, size, agileUserVO),
                new TypeReference<Page<UserDTO>>() {
                });
    }

    public OrganizationInfoVO query(Long id) {
        return ResponseUtils.getResponse(remoteIamFeignClient.query(id), OrganizationInfoVO.class);
    }

    public Page<UserDTO> agileUsersByProjectIds(Long projectId, int page, int size, AgileUserVO agileUserVO) {
        return ResponseUtils.getResponse(remoteIamFeignClient.agileUsersByProjectIds(projectId, page, size, agileUserVO),
                new TypeReference<Page<UserDTO>>() {
                });
    }

    public List<RoleVO> listRolesByIds(Long tenantId, List<Long> roleIds) {
        return ResponseUtils.getResponse(remoteIamFeignClient.listRolesByIds(tenantId, roleIds),
                new TypeReference<List<RoleVO>>() {
                });
    }

    /*
     * 分页查询组织下的项目
     * @param organizationId
     * @param size
     * @param code
     * @param name
     * @param page
     * @param enabled
     * @param params
     * @return
     */
    public Page<ProjectVO> pagedQueryProjects(Long organizationId, int page, int size, String name, String code, Boolean enabled, Boolean withAdditionInfo, String params) {
        return ResponseUtils.getResponse(remoteIamFeignClient.pagedQueryProjects(organizationId, page, size, name, code, enabled, withAdditionInfo, params),
                new TypeReference<Page<ProjectVO>>() {
                });
    }

    public Page<UserDTO> pagingQueryUsersOnOrganizationAgile(Long id, int page, int size, Long userId, String email, String param, List<Long> notSelectUserIds) {
        return ResponseUtils.getResponse(remoteIamFeignClient.pagingQueryUsersOnOrganizationAgile(id, page, size, userId, email, param, notSelectUserIds),
                new TypeReference<Page<UserDTO>>() {
                });
    }

    public TimeZoneWorkCalendarRefDetailVO queryTimeZoneWorkCalendarDetail(Long organizationId, Integer year) {
        return ResponseUtils.getResponse(remoteIamFeignClient.queryTimeZoneWorkCalendarDetail(organizationId, year), TimeZoneWorkCalendarRefDetailVO.class);
    }

    public List<ProjectVO> listProjectsByUserIdForSimple(Long organizationId, Long userId, String category, Boolean enabled) {
        return ResponseUtils.getResponse(remoteIamFeignClient.listProjectsByUserIdForSimple(organizationId, userId, category, enabled),
                new TypeReference<List<ProjectVO>>() {
                });
    }

    public Page<ProjectVO> pagingQueryProjectsByUserId(Long organizationId, Long userId, Integer page, Integer size, ProjectVO projectVO) {
        return ResponseUtils.getResponse(remoteIamFeignClient.pagingQueryProjectsByUserId(organizationId, userId, page, size, projectVO),
                new TypeReference<Page<ProjectVO>>() {
                });
    }

}
