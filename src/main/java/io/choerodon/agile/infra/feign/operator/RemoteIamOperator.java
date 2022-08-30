package io.choerodon.agile.infra.feign.operator;

import java.util.List;
import java.util.Set;

import com.fasterxml.jackson.core.type.TypeReference;
import org.springframework.stereotype.Component;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.infra.dto.TimeZoneWorkCalendarDTO;
import io.choerodon.agile.infra.dto.UserDTO;
import io.choerodon.agile.infra.feign.IamFeignClient;
import io.choerodon.agile.infra.feign.vo.OrganizationInfoVO;
import io.choerodon.core.domain.Page;

import org.hzero.core.util.ResponseUtils;

/**
 * Copyright (c) 2022. Hand Enterprise Solution Company. All right reserved.
 *
 * @author zongqi.hao@zknow.com
 * @since 2022/7/14
 */
@Component
public class RemoteIamOperator {

    private final IamFeignClient iamFeignClient;

    public RemoteIamOperator(IamFeignClient iamFeignClient) {
        this.iamFeignClient = iamFeignClient;
    }

    public List<ProjectVO> queryProjectByIds(Set<Long> ids) {
        return ResponseUtils.getResponse(iamFeignClient.queryProjectByIds(ids),
                new TypeReference<List<ProjectVO>>() {
                });
    }

    public UserDTO query(Long organizationId, Long id) {
        return ResponseUtils.getResponse(iamFeignClient.query(organizationId, id), UserDTO.class);
    }

    public List<UserDTO> listUsersByIds(Long[] ids, Boolean onlyEnabled) {
        return ResponseUtils.getResponse(iamFeignClient.listUsersByIds(ids, onlyEnabled),
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
        return ResponseUtils.getResponse(iamFeignClient.queryProject(id), ProjectVO.class);
    }

    /**
     * 按照Id查询项目
     *
     * @param id 要查询的项目ID
     * @return 查询到的项目
     */
    public ProjectVO queryProject(Long id, Boolean withAgileInfo) {
        return ResponseUtils.getResponse(iamFeignClient.queryProject(id, withAgileInfo), ProjectVO.class);
    }

    public Page<UserDTO> listUsersByProjectId(Long id, int page, int size, String param) {
        return ResponseUtils.getResponse(iamFeignClient.listUsersByProjectId(id, page, size, param),
                new TypeReference<Page<UserDTO>>() {
                });
    }


    public Page<UserDTO> agileUsers(Long projectId, int page, int size, AgileUserVO agileUserVO) {
        return ResponseUtils.getResponse(iamFeignClient.agileUsers(projectId, page, size, agileUserVO),
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
        return ResponseUtils.getResponse(iamFeignClient.list(id, param),
                new TypeReference<Page<UserVO>>() {
                });
    }

    public List<RoleVO> listRolesWithUserCountOnProjectLevel(Long sourceId, RoleAssignmentSearchVO roleAssignmentSearchVO) {
        return ResponseUtils.getResponse(iamFeignClient.listRolesWithUserCountOnProjectLevel(sourceId, roleAssignmentSearchVO),
                new TypeReference<List<RoleVO>>() {
                });
    }

    public Page<UserVO> pagingQueryUsersByRoleIdOnProjectLevel(int page, int size,
                                                               Long roleId,
                                                               Boolean doPage,
                                                               Long sourceId,
                                                               RoleAssignmentSearchVO roleAssignmentSearchVO) {
        return ResponseUtils.getResponse(iamFeignClient.pagingQueryUsersByRoleIdOnProjectLevel(page, size, roleId, doPage, sourceId, roleAssignmentSearchVO),
                new TypeReference<Page<UserVO>>() {
                });
    }

    public Page<UserWithRoleVO> pagingQueryUsersWithProjectLevelRoles(int page, int size,
                                                                      Long sourceId,
                                                                      RoleAssignmentSearchVO roleAssignmentSearchVO,
                                                                      boolean doPage) {
        return ResponseUtils.getResponse(iamFeignClient.pagingQueryUsersWithProjectLevelRoles(page, size, sourceId, roleAssignmentSearchVO, doPage),
                new TypeReference<Page<UserWithRoleVO>>() {
                });
    }

    /**
     * 根据组织id查询所有项目
     *
     * @param organizationId organizationId
     * @return result
     */
    public List<ProjectVO> listProjectsByOrgId(Long organizationId) {
        return ResponseUtils.getResponse(iamFeignClient.listProjectsByOrgId(organizationId),
                new TypeReference<List<ProjectVO>>() {
                });
    }

    public Page<ProjectVO> listWithCategoryByOrganizationIds(Long organizationId,
                                                      ProjectSearchVO projectSearchVO,
                                                      Integer page,
                                                      Integer size) {
        return ResponseUtils.getResponse(iamFeignClient.listWithCategoryByOrganizationIds(organizationId, projectSearchVO, page, size),
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
        return ResponseUtils.getResponse(iamFeignClient.queryTimeZoneDetailByOrganizationId(organizationId), TimeZoneWorkCalendarDTO.class);
    }

    /**
     * 根据年份查询工作日历假期(包含查询年份和下一年份数据)
     *
     * @param organizationId 租户id
     * @param year           年份
     * @return 日历假期信息
     */
    public List<WorkCalendarHolidayRefVO> queryWorkCalendarHolidayRelByYear(Long organizationId, Integer year) {
        return ResponseUtils.getResponse(iamFeignClient.queryWorkCalendarHolidayRelByYear(organizationId, year),
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
        return ResponseUtils.getResponse(iamFeignClient.queryByYearIncludeLastAndNext(organizationId, year),
                new TypeReference<List<WorkCalendarHolidayRefVO>>() {
                });
    }


    public List<RoleVO> getUserWithProjLevelRolesByUserId(Long projectId, Long userId) {
        return ResponseUtils.getResponse(iamFeignClient.getUserWithProjLevelRolesByUserId(projectId, userId),
                new TypeReference<List<RoleVO>>() {
                });
    }

    public List<UserVO> listUsersByRealNames(Boolean onlyEnabled, Set<String> realNames) {
        return ResponseUtils.getResponse(iamFeignClient.listUsersByRealNames(onlyEnabled, realNames),
                new TypeReference<List<UserVO>>() {
                });
    }

    public List<UserVO> listProjectOwnerById(Long projectId) {
        return ResponseUtils.getResponse(iamFeignClient.listProjectOwnerById(projectId),
                new TypeReference<List<UserVO>>() {
                });
    }

    public List<ProjectWithUserVO> listProjectOwnerByIds(Set<Long> projectIds) {
        return ResponseUtils.getResponse(iamFeignClient.listProjectOwnerByIds(projectIds),
                new TypeReference<List<ProjectWithUserVO>>() {
                });
    }

    public List<UserVO> listUsersUnderRoleByIds(Long projectId, String roleIdString) {
        return ResponseUtils.getResponse(iamFeignClient.listUsersUnderRoleByIds(projectId, roleIdString),
                new TypeReference<List<UserVO>>() {
                });
    }

    public Boolean checkIsProjectOwner(Long id, Long projectId) {
        return ResponseUtils.getResponse(iamFeignClient.checkIsProjectOwner(id, projectId), Boolean.class);
    }

    public List<ProjectVO> queryProjects(Long id, boolean includedDisabled) {
        return ResponseUtils.getResponse(iamFeignClient.queryProjects(id, includedDisabled),
                new TypeReference<List<ProjectVO>>() {
                });
    }

    public List<ProjectVO> queryOrgProjects(Long organizationId, Long userId) {
        return ResponseUtils.getResponse(iamFeignClient.queryOrgProjects(organizationId, userId),
                new TypeReference<List<ProjectVO>>() {
                });
    }

    public List<RoleVO> listProjectRoles(Long projectId, Boolean onlySelectEnable, String roleName) {
        return ResponseUtils.getResponse(iamFeignClient.listProjectRoles(projectId, onlySelectEnable, roleName),
                new TypeReference<List<RoleVO>>() {
                });
    }

    public Page<RoleVO> listOrganizationRoles(Integer page, Integer size, String name, String code, String roleLevel, Long tenantId, Boolean builtIn, Boolean enabled, String params) {
        return ResponseUtils.getResponse(iamFeignClient.listOrganizationRoles(page, size, name, code, roleLevel, tenantId, builtIn, enabled, params),
                new TypeReference<Page<RoleVO>>() {
                });
    }

    public Page<UserVO> queryUsersByProject(Long projectId, String param, int page, int size) {
        return ResponseUtils.getResponse(iamFeignClient.queryUsersByProject(projectId, param, page, size),
                new TypeReference<Page<UserVO>>() {
                });
    }

    public Page<UserVO> queryUsersByOrganization(Long projectId, String param, int page, int size) {
        return ResponseUtils.getResponse(iamFeignClient.queryUsersByOrganization(projectId, param, page, size),
                new TypeReference<Page<UserVO>>() {
                });
    }

    public Page<UserDTO> pagingUsersOnOrganizationLevel(Long organizationId, int page, int size, AgileUserVO agileUserVO) {
        return ResponseUtils.getResponse(iamFeignClient.pagingUsersOnOrganizationLevel(organizationId, page, size, agileUserVO),
                new TypeReference<Page<UserDTO>>() {
                });
    }

    public OrganizationInfoVO query(Long id) {
        return ResponseUtils.getResponse(iamFeignClient.query(id), OrganizationInfoVO.class);
    }

    public Page<UserDTO> agileUsersByProjectIds(Long projectId, int page, int size, AgileUserVO agileUserVO) {
        return ResponseUtils.getResponse(iamFeignClient.agileUsersByProjectIds(projectId, page, size, agileUserVO),
                new TypeReference<Page<UserDTO>>() {
                });
    }

    public List<RoleVO> listRolesByIds(Long tenantId, List<Long> roleIds) {
        return ResponseUtils.getResponse(iamFeignClient.listRolesByIds(tenantId, roleIds),
                new TypeReference<List<RoleVO>>() {
                });
    }

    /*
     * 分页查询组织下的项目
     * @param organizationId organizationId
     * @param size size
     * @param code code
     * @param name name
     * @param page page
     * @param enabled enabled
     * @param params params
     * @return result
     */
    public Page<ProjectVO> pagedQueryProjects(Long organizationId, int page, int size, String name, String code, Boolean enabled, Boolean withAdditionInfo, String params) {
        return ResponseUtils.getResponse(iamFeignClient.pagedQueryProjects(organizationId, page, size, name, code, enabled, withAdditionInfo, params),
                new TypeReference<Page<ProjectVO>>() {
                });
    }

    public Page<UserDTO> pagingQueryUsersOnOrganizationAgile(Long id, int page, int size, Long userId, String email, String param, List<Long> notSelectUserIds) {
        return ResponseUtils.getResponse(iamFeignClient.pagingQueryUsersOnOrganizationAgile(id, page, size, userId, email, param, notSelectUserIds),
                new TypeReference<Page<UserDTO>>() {
                });
    }

    public TimeZoneWorkCalendarRefDetailVO queryTimeZoneWorkCalendarDetail(Long organizationId, Integer year) {
        return ResponseUtils.getResponse(iamFeignClient.queryTimeZoneWorkCalendarDetail(organizationId, year), TimeZoneWorkCalendarRefDetailVO.class);
    }

    public List<ProjectVO> listProjectsByUserIdForSimple(Long organizationId, Long userId, String category, Boolean enabled) {
        return ResponseUtils.getResponse(iamFeignClient.listProjectsByUserIdForSimple(organizationId, userId, category, enabled),
                new TypeReference<List<ProjectVO>>() {
                });
    }

    public Page<ProjectVO> pagingQueryProjectsByUserId(Long organizationId, Long userId, Integer page, Integer size, ProjectVO projectVO) {
        return ResponseUtils.getResponse(iamFeignClient.pagingQueryProjectsByUserId(organizationId, userId, page, size, projectVO),
                new TypeReference<Page<ProjectVO>>() {
                });
    }

}
