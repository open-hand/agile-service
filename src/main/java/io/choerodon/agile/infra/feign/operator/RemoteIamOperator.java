package io.choerodon.agile.infra.feign.operator;

import java.util.Collection;
import java.util.List;
import java.util.Set;
import java.util.concurrent.TimeUnit;

import com.fasterxml.jackson.core.type.TypeReference;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.hzero.core.util.ResponseUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Component;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.infra.dto.TimeZoneWorkCalendarDTO;
import io.choerodon.agile.infra.dto.UserDTO;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.feign.IamFeignClient;
import io.choerodon.agile.infra.feign.vo.OrganizationInfoVO;
import io.choerodon.core.domain.Page;
import io.choerodon.core.oauth.DetailsHelper;

/**
 * Copyright (c) 2022. Hand Enterprise Solution Company. All right reserved.
 *
 * @author zongqi.hao@zknow.com
 * @since 2022/7/14
 */
@Component
public class RemoteIamOperator {

    private static final String USER_OF_ORG_RESULT_REDIS_KEY = "org:%s:user:%s";

    private final IamFeignClient iamFeignClient;
    private final BaseFeignClient baseFeignClient;

    public RemoteIamOperator(IamFeignClient iamFeignClient,
                             BaseFeignClient baseFeignClient) {
        this.iamFeignClient = iamFeignClient;
        this.baseFeignClient = baseFeignClient;
    }

    @Autowired
    private StringRedisTemplate stringRedisTemplate;

    /**
     * 分页查询租户信息
     *
     * @param tenantName 模糊查询条件--租户名称
     * @param tenantNum  模糊查询条件--租户编码
     * @return 查询结果
     */
    public Page<TenantVO> pagingTenants(String tenantName, String tenantNum, int page, int size) {
        return ResponseUtils.getResponse(
                this.iamFeignClient.pagingTenants(tenantName, tenantNum, page, size),
                new TypeReference<Page<TenantVO>>() {
                }
        );
    }

    /**
     * 根据租户编码查询租户信息<br/>
     * <span style="color:red">注意, 这个是通过模糊查询接口获取的结果, 当租户编码区分度太低时也许会出问题, 慎用</span>
     *
     * @param tenantNum 租户名称
     * @return 查询结果
     */
    public TenantVO findTenantByNumber(String tenantNum) {
        if (StringUtils.isBlank(tenantNum)) {
            return null;
        }
        final Page<TenantVO> page = ResponseUtils.getResponse(
                this.iamFeignClient.pagingTenants(null, tenantNum, 0, 1000),
                new TypeReference<Page<TenantVO>>() {
                }
        );
        if (CollectionUtils.isEmpty(page)) {
            return null;
        }
        return page.stream().filter(t -> tenantNum.equals(t.getTenantNum())).findFirst().orElse(null);
    }

    public List<ProjectVO> queryProjectByIds(Set<Long> ids) {
        return ResponseUtils.getResponse(baseFeignClient.queryProjectByIds(ids),
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
        return ResponseUtils.getResponse(baseFeignClient.queryProject(id), ProjectVO.class);
    }

    /**
     * 按照Id查询项目
     *
     * @param id 要查询的项目ID
     * @return 查询到的项目
     */
    public ProjectVO queryProject(Long id, Boolean withAgileInfo) {
        return ResponseUtils.getResponse(baseFeignClient.queryProject(id, withAgileInfo), ProjectVO.class);
    }

    public Page<UserDTO> listUsersByProjectId(Long id, int page, int size, String param) {
        return ResponseUtils.getResponse(baseFeignClient.listUsersByProjectId(id, page, size, param),
                new TypeReference<Page<UserDTO>>() {
                });
    }


    public Page<UserDTO> agileUsers(Long projectId, int page, int size, AgileUserVO agileUserVO) {
        return ResponseUtils.getResponse(baseFeignClient.agileUsers(projectId, page, size, agileUserVO),
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
        return ResponseUtils.getResponse(baseFeignClient.list(id, param),
                new TypeReference<Page<UserVO>>() {
                });
    }

    public List<RoleVO> listRolesWithUserCountOnProjectLevel(Long sourceId, RoleAssignmentSearchVO roleAssignmentSearchVO) {
        return ResponseUtils.getResponse(baseFeignClient.listRolesWithUserCountOnProjectLevel(sourceId, roleAssignmentSearchVO),
                new TypeReference<List<RoleVO>>() {
                });
    }

    public Page<UserVO> pagingQueryUsersByRoleIdOnProjectLevel(int page, int size,
                                                               Long roleId,
                                                               Boolean doPage,
                                                               Long sourceId,
                                                               RoleAssignmentSearchVO roleAssignmentSearchVO) {
        return ResponseUtils.getResponse(baseFeignClient.pagingQueryUsersByRoleIdOnProjectLevel(page, size, roleId, doPage, sourceId, roleAssignmentSearchVO),
                new TypeReference<Page<UserVO>>() {
                });
    }

    public Page<UserWithRoleVO> pagingQueryUsersWithProjectLevelRoles(int page, int size,
                                                                      Long sourceId,
                                                                      RoleAssignmentSearchVO roleAssignmentSearchVO,
                                                                      boolean doPage) {
        return ResponseUtils.getResponse(baseFeignClient.pagingQueryUsersWithProjectLevelRoles(page, size, sourceId, roleAssignmentSearchVO, doPage),
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
        return ResponseUtils.getResponse(baseFeignClient.listProjectsByOrgId(organizationId),
                new TypeReference<List<ProjectVO>>() {
                });
    }

    /**
     * 查询组织下所有项目
     *
     * @param organizationId 组织ID
     * @param projectCodes   过滤条件--项目编码
     * @param enabledFlag    过滤条件--是否启用
     * @return 查询结果
     */
    public List<ProjectVO> listProjectsByOrgId(Long organizationId, Collection<String> projectCodes, Boolean enabledFlag) {
        return ResponseUtils.getResponse(baseFeignClient.listProjectsByOrgId(organizationId, projectCodes, enabledFlag),
                new TypeReference<List<ProjectVO>>() {
                });
    }

    public Page<ProjectVO> listWithCategoryByOrganizationIds(Long organizationId,
                                                             ProjectSearchVO projectSearchVO,
                                                             Integer page,
                                                             Integer size) {
        return ResponseUtils.getResponse(baseFeignClient.listWithCategoryByOrganizationIds(organizationId, projectSearchVO, page, size),
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
        return ResponseUtils.getResponse(baseFeignClient.queryTimeZoneDetailByOrganizationId(organizationId), TimeZoneWorkCalendarDTO.class);
    }

    /**
     * 根据年份查询工作日历假期(包含查询年份和下一年份数据)
     *
     * @param organizationId 租户id
     * @param year           年份
     * @return 日历假期信息
     */
    public List<WorkCalendarHolidayRefVO> queryWorkCalendarHolidayRelByYear(Long organizationId, Integer year) {
        return ResponseUtils.getResponse(baseFeignClient.queryWorkCalendarHolidayRelByYear(organizationId, year),
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
        return ResponseUtils.getResponse(baseFeignClient.queryByYearIncludeLastAndNext(organizationId, year),
                new TypeReference<List<WorkCalendarHolidayRefVO>>() {
                });
    }


    public List<RoleVO> getUserWithProjLevelRolesByUserId(Long projectId, Long userId) {
        return ResponseUtils.getResponse(baseFeignClient.getUserWithProjLevelRolesByUserId(projectId, userId),
                new TypeReference<List<RoleVO>>() {
                });
    }

    public List<UserVO> listUsersByRealNames(Boolean onlyEnabled, Set<String> realNames) {
        return ResponseUtils.getResponse(iamFeignClient.listUsersByRealNames(onlyEnabled, realNames),
                new TypeReference<List<UserVO>>() {
                });
    }

    public List<UserVO> listProjectOwnerById(Long projectId) {
        return ResponseUtils.getResponse(baseFeignClient.listProjectOwnerById(projectId),
                new TypeReference<List<UserVO>>() {
                });
    }

    public List<ProjectWithUserVO> listProjectOwnerByIds(Set<Long> projectIds) {
        return ResponseUtils.getResponse(baseFeignClient.listProjectOwnerByIds(projectIds),
                new TypeReference<List<ProjectWithUserVO>>() {
                });
    }

    public List<UserVO> listUsersUnderRoleByIds(Long projectId, String roleIdString) {
        return ResponseUtils.getResponse(baseFeignClient.listUsersUnderRoleByIds(projectId, roleIdString),
                new TypeReference<List<UserVO>>() {
                });
    }

    public Boolean checkIsProjectOwner(Long id, Long projectId) {
        return ResponseUtils.getResponse(baseFeignClient.checkIsProjectOwner(id, projectId), Boolean.class);
    }

    public List<ProjectVO> queryProjects(Long id, boolean includedDisabled) {
        return ResponseUtils.getResponse(baseFeignClient.queryProjects(id, includedDisabled),
                new TypeReference<List<ProjectVO>>() {
                });
    }

    public List<ProjectVO> queryOrgProjects(Long organizationId, Long userId) {
        return ResponseUtils.getResponse(baseFeignClient.queryOrgProjects(organizationId, userId),
                new TypeReference<List<ProjectVO>>() {
                });
    }

    public List<RoleVO> listProjectRoles(Long projectId, Boolean onlySelectEnable, String roleName) {
        return ResponseUtils.getResponse(baseFeignClient.listProjectRoles(projectId, onlySelectEnable, roleName),
                new TypeReference<List<RoleVO>>() {
                });
    }

    public Page<RoleVO> listOrganizationRoles(Integer page, Integer size, String name, String code, String roleLevel, Long tenantId, Boolean builtIn, Boolean enabled, String params) {
        return ResponseUtils.getResponse(baseFeignClient.listOrganizationRoles(page, size, name, code, roleLevel, tenantId, builtIn, enabled, params),
                new TypeReference<Page<RoleVO>>() {
                });
    }

    public Page<UserVO> queryUsersByProject(Long projectId, String param, int page, int size) {
        return ResponseUtils.getResponse(baseFeignClient.queryUsersByProject(projectId, param, page, size),
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
        return ResponseUtils.getResponse(baseFeignClient.agileUsersByProjectIds(projectId, page, size, agileUserVO),
                new TypeReference<Page<UserDTO>>() {
                });
    }

    public List<RoleVO> listRolesByIds(Long tenantId, List<Long> roleIds) {
        return ResponseUtils.getResponse(baseFeignClient.listRolesByIds(tenantId, roleIds),
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
        return ResponseUtils.getResponse(baseFeignClient.pagedQueryProjects(organizationId, page, size, name, code, enabled, withAdditionInfo, params),
                new TypeReference<Page<ProjectVO>>() {
                });
    }

    public Page<UserDTO> pagingQueryUsersOnOrganizationAgile(Long id, int page, int size, Long userId, String email, String param, List<Long> notSelectUserIds) {
        return ResponseUtils.getResponse(iamFeignClient.pagingQueryUsersOnOrganizationAgile(id, page, size, userId, email, param, notSelectUserIds),
                new TypeReference<Page<UserDTO>>() {
                });
    }

    public TimeZoneWorkCalendarRefDetailVO queryTimeZoneWorkCalendarDetail(Long organizationId, Integer year) {
        return ResponseUtils.getResponse(baseFeignClient.queryTimeZoneWorkCalendarDetail(organizationId, year), TimeZoneWorkCalendarRefDetailVO.class);
    }

    public List<ProjectVO> listProjectsByUserIdForSimple(Long organizationId, Long userId, String category, Boolean enabled) {
        return ResponseUtils.getResponse(baseFeignClient.listProjectsByUserIdForSimple(organizationId, userId, category, enabled),
                new TypeReference<List<ProjectVO>>() {
                });
    }

    public Page<ProjectVO> pagingQueryProjectsByUserId(Long organizationId, Long userId, Integer page, Integer size, ProjectVO projectVO) {
        return ResponseUtils.getResponse(baseFeignClient.pagingQueryProjectsByUserId(organizationId, userId, page, size, projectVO),
                new TypeReference<Page<ProjectVO>>() {
                });
    }

    /**
     * 根据项目id查询用户在项目所属组织下是否有角色
     *
     * @param projectId
     * @return
     */
    public Boolean memberOfOrganization(Long projectId) {
        ProjectVO projectVO = queryProject(projectId);
        Long userId = DetailsHelper.getUserDetails().getUserId();
        String cacheKey = String.format(USER_OF_ORG_RESULT_REDIS_KEY, projectVO.getOrganizationId(), userId);

        String cacheResult = stringRedisTemplate.opsForValue().get(cacheKey);
        if (cacheResult == null) {
            Boolean result = ResponseUtils.getResponse(baseFeignClient.memberOfOrganization(projectVO.getOrganizationId(), userId), Boolean.class);
            stringRedisTemplate.opsForValue().set(String.format(USER_OF_ORG_RESULT_REDIS_KEY, projectVO.getOrganizationId(), userId), result.toString(), 60, TimeUnit.SECONDS);
            return result;
        } else {
            return Boolean.valueOf(cacheResult);
        }
    }

}
