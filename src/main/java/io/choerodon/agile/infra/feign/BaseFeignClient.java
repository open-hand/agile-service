package io.choerodon.agile.infra.feign;

import io.choerodon.agile.infra.feign.vo.OrganizationInfoVO;
import io.choerodon.core.domain.Page;
import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.infra.dto.TimeZoneWorkCalendarDTO;
import io.choerodon.agile.infra.dto.UserDTO;
import io.choerodon.agile.infra.feign.fallback.BaseFeignClientFallback;
import io.swagger.annotations.ApiParam;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;
import java.util.Set;

/**
 * @author dinghuang123@gmail.com
 * @since 2018/5/24
 */
@Component
@FeignClient(value = "choerodon-iam", fallback = BaseFeignClientFallback.class)
public interface BaseFeignClient {

    /**
     * 查询用户信息
     *
     * @param organizationId organizationId
     * @param id             id
     * @return UserDTO
     */
    @GetMapping(value = "/choerodon/v1/organizations/{organization_id}/users/{id}")
    ResponseEntity<UserDTO> query(@PathVariable(name = "organization_id") Long organizationId,
                                  @PathVariable("id") Long id);

    @PostMapping(value = "/choerodon/v1/users/ids")
    ResponseEntity<List<UserDTO>> listUsersByIds(@RequestBody Long[] ids,
                                                 @RequestParam(name = "only_enabled") Boolean onlyEnabled);

    /**
     * 按照Id查询项目
     *
     * @param id 要查询的项目ID
     * @return 查询到的项目
     */
    @GetMapping(value = "/choerodon/v1/projects/{id}")
    ResponseEntity<ProjectVO> queryProject(@PathVariable("id") Long id);

    /**
     * 按照Id查询项目
     *
     * @param id 要查询的项目ID
     * @return 查询到的项目
     */
    @GetMapping(value = "/choerodon/v1/projects/{id}")
    ResponseEntity<ProjectVO> queryProject(@PathVariable("id") Long id,
                                           @RequestParam(value = "with_agile_info") Boolean withAgileInfo);

    @GetMapping(value = "/choerodon/v1/projects/{id}/users")
    ResponseEntity<Page<UserDTO>> listUsersByProjectId(@PathVariable("id") Long id,
                                                       @RequestParam("page") int page,
                                                       @RequestParam("size") int size,
                                                       @RequestParam("param") String param);


    @PostMapping(value = "/choerodon/v1/projects/{id}/agile_users")
    ResponseEntity<Page<UserDTO>> agileUsers(@PathVariable("id") Long projectId,
                                             @RequestParam("page") int page,
                                             @RequestParam("size") int size,
                                             @RequestBody AgileUserVO agileUserVO);

    /**
     * 根据projectId和param模糊查询loginName和realName两列
     *
     * @param id    id
     * @param param param
     * @return UserVO
     */
    @GetMapping(value = "/choerodon/v1/projects/{id}/users")
    ResponseEntity<Page<UserVO>> list(@PathVariable("id") Long id,
                                          @RequestParam("param") String param);

    @PostMapping(value = "/choerodon/v1/projects/{project_id}/role_members/users/count")
    ResponseEntity<List<RoleVO>> listRolesWithUserCountOnProjectLevel(
            @PathVariable(name = "project_id") Long sourceId,
            @RequestBody(required = false) @Valid RoleAssignmentSearchVO roleAssignmentSearchVO);

    @PostMapping(value = "/choerodon/v1/projects/{project_id}/role_members/users")
    ResponseEntity<Page<UserVO>> pagingQueryUsersByRoleIdOnProjectLevel(
            @RequestParam(name = "page") int page,
            @RequestParam(name = "size") int size,
            @RequestParam(name = "role_id", required = false) Long roleId,
            @RequestParam Boolean doPage,
            @PathVariable(name = "project_id") Long sourceId,
            @RequestBody(required = false) @Valid RoleAssignmentSearchVO roleAssignmentSearchVO);

    @GetMapping(value = "/choerodon/v1/organizations/{organization_id}/projects/{project_id}/program")
    ResponseEntity<ProjectVO> getGroupInfoByEnableProject(@PathVariable(name = "organization_id") Long organizationId,
                                                          @PathVariable(name = "project_id") Long projectId);

    @PostMapping(value = "/choerodon/v1/organizations/{organization_id}/projects/programs")
    ResponseEntity<List<ProjectVO>> getGroupInfoByEnableProjects(@PathVariable(name = "organization_id") Long organizationId,
                                                                 @RequestBody Set<Long> projectIds);

    @PostMapping(value = "/choerodon/v1/projects/{project_id}/role_members/users/roles")
    ResponseEntity<Page<UserWithRoleVO>> pagingQueryUsersWithProjectLevelRoles(
            @RequestParam(name = "page") int page,
            @RequestParam(name = "size") int size,
            @PathVariable(name = "project_id") Long sourceId,
            @RequestBody(required = false) @Valid RoleAssignmentSearchVO roleAssignmentSearchVO,
            @RequestParam(name = "doPage") boolean doPage);

    /**
     * 根据组织id查询所有项目
     *
     * @param organizationId
     * @return
     */
    @GetMapping(value = "/choerodon/v1/organizations/{organization_id}/projects/all")
    ResponseEntity<List<ProjectVO>> listProjectsByOrgId(@PathVariable("organization_id") Long organizationId);

    /**
     *
     * @param organizationId
     * @return
     */
    @PostMapping(value = "/choerodon/v1/organizations/{organization_id}/projects/all_with_category")
    ResponseEntity<Page<ProjectVO>> listWithCategoryByOrganizationIds(@PathVariable("organization_id") Long organizationId,
                                                                      @RequestBody ProjectSearchVO projectSearchVO,
                                                                      @RequestParam Integer page,
                                                                      @RequestParam Integer size);


    /**
     * 根据组织id获取时区工作日历
     *
     * @param organizationId
     * @return
     */
    @GetMapping(value = "/choerodon/v1/organizations/{organization_id}/time_zone_work_calendars/query_by_org_id")
    ResponseEntity<TimeZoneWorkCalendarDTO> queryTimeZoneDetailByOrganizationId(@ApiParam(value = "组织id", required = true)
                                                                                @PathVariable(name = "organization_id") Long organizationId);

    /**
     * 根据年份查询工作日历假期(包含查询年份和下一年份数据)
     *
     * @param organizationId
     * @param year
     * @return
     */
    @GetMapping(value = "/choerodon/v1/organizations/{organization_id}/work_calendar_holiday_refs")
    ResponseEntity<List<WorkCalendarHolidayRefVO>> queryWorkCalendarHolidayRelByYear(@ApiParam(value = "项目id", required = true)
                                                                                     @PathVariable(name = "organization_id") Long organizationId,
                                                                                     @ApiParam(value = "要查询的年份", required = true)
                                                                                     @RequestParam(name = "year") Integer year);

    /**
     * 根据年份查询工作日历，包含当年、去年、明年
     *
     * @param organizationId
     * @param year
     * @return
     */
    @GetMapping(value = "/choerodon/v1/organizations/{organization_id}/work_calendar_holiday_refs/year_include_last_and_next")
    ResponseEntity<List<WorkCalendarHolidayRefVO>> queryByYearIncludeLastAndNext(@ApiParam(value = "项目id", required = true)
                                                                                 @PathVariable(name = "organization_id") Long organizationId,
                                                                                 @ApiParam(value = "要查询的年份", required = true)
                                                                                 @RequestParam(name = "year") Integer year);



    @GetMapping(value = "/choerodon/v1/projects/{project_id}/role_members/users/{user_id}")
    ResponseEntity<List<RoleVO>> getUserWithProjLevelRolesByUserId(@PathVariable(name = "project_id") Long projectId,
                                                                   @PathVariable(name = "user_id") Long userId);

    @PostMapping(value = "/choerodon/v1/users/real_names")
    ResponseEntity<List<UserVO>> listUsersByRealNames(@RequestParam(name = "only_enabled") Boolean onlyEnabled,
                                                      @RequestBody Set<String> realNames);

    @GetMapping(value = "/choerodon/v1/projects/{project_id}/owner/list")
    ResponseEntity<List<UserVO>> listProjectOwnerById(@PathVariable(name = "project_id") Long projectId);

    @GetMapping("/choerodon/v1/users/{id}/projects/{project_id}/check_is_owner")
    ResponseEntity<Boolean> checkIsProjectOwner(@PathVariable("id") Long id, @PathVariable("project_id") Long projectId);

    @GetMapping("choerodon/v1/users/{id}/projects")
    ResponseEntity<List<ProjectVO>> queryProjects( @PathVariable("id") Long id,
                                                          @RequestParam(required = false, name = "included_disabled")
                                                          boolean includedDisabled);
    @GetMapping("/choerodon/v1/organizations/{organization_id}/users/{user_id}/projects")
    ResponseEntity<List<ProjectVO>> queryOrgProjects(@PathVariable("organization_id") Long organizationId,
                                                      @PathVariable("user_id") Long userId);

    @GetMapping("/choerodon/v1/organizations/{organization_id}/users/{user_id}/projects/paging")
    ResponseEntity<Page<ProjectVO>> pagingProjectsByUserId(@PathVariable("organization_id") Long organizationId,
                                                           @PathVariable("user_id") Long userId,
                                                           @RequestParam int page,
                                                           @RequestParam int size,
                                                           @RequestParam(required = false) Boolean enabled,
                                                           @RequestParam(required = false) String category);

    @PostMapping(value = "/choerodon/v1/projects/ids")
    ResponseEntity<List<ProjectVO>> queryByIds(@RequestBody Set<Long> ids);

    @GetMapping(value = "/choerodon/v1/projects/{project_id}/roles")
    ResponseEntity<List<RoleVO>> listProjectRoles(@PathVariable("project_id") Long projectId,
                                                  @RequestParam(name = "only_select_enable") Boolean onlySelectEnable,
                                                  @RequestParam(name = "role_name") String roleName);

    @GetMapping(value = "/choerodon/v1/projects/{project_id}/users")
    ResponseEntity<Page<UserVO>> queryUsersByProject(@PathVariable("project_id") Long projectId,
                                                     @RequestParam("param") String param,
                                                     @RequestParam int page,
                                                     @RequestParam int size);

    @GetMapping(value = "/choerodon/v1/organizations/{organization_id}/users")
    ResponseEntity<Page<UserVO>> queryUsersByOrganization(@PathVariable("organization_id") Long projectId,
                                                          @RequestParam("param") String param,
                                                          @RequestParam int page,
                                                          @RequestParam int size);

    @PostMapping(value = "/choerodon/v1/organizations/{organization_id}/users/page")
    ResponseEntity<Page<UserDTO>> pagingUsersOnOrganizationLevel(@PathVariable(name = "organization_id") Long organizationId,
                                                                 @RequestParam int page,
                                                                 @RequestParam int size,
                                                                 @RequestBody(required = false) AgileUserVO agileUserVO);

    @PostMapping(value = "/choerodon/v1/inner/projects/list_owner")
    ResponseEntity<List<ProjectWithUserVO>> listProjectOwnerByIds(@RequestBody Set<Long> projectIds);

    @GetMapping(value = "/choerodon/v1/organizations/{organization_id}")
    ResponseEntity<OrganizationInfoVO> query(@PathVariable(name = "organization_id") Long id);

    @PostMapping(value = "/choerodon/v1/projects/{project_id}/agile_users_by_projects")
    ResponseEntity<Page<UserDTO>> agileUsersByProjectIds(@PathVariable(name = "project_id") Long projectId,
                                                         @RequestParam("page") int page,
                                                         @RequestParam("size") int size,
                                                         @RequestBody AgileUserVO agileUserVO);

    @PostMapping(value = "/choerodon/v1/list_roles")
    ResponseEntity<List<RoleVO>> listRolesByIds(@RequestParam("tenantId") Long  tenantId,
                                                @RequestBody List<Long> roleIds);

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
    @GetMapping(value = "/choerodon/v1/organizations/{organization_id}/projects")
    ResponseEntity<Page<ProjectVO>> pagedQueryProjects(@PathVariable(name = "organization_id") Long organizationId,
                                                       @RequestParam(name = "page") int page,
                                                       @RequestParam(name = "size") int size,
                                                       @RequestParam String name,
                                                       @RequestParam String code,
                                                       @RequestParam Boolean enabled,
                                                       @RequestParam Boolean withAdditionInfo,
                                                       @RequestParam String params);

    @PostMapping(value = "/choerodon/v1/projects/ids")
    ResponseEntity<List<ProjectVO>> queryProjectByIds(@RequestBody Set<Long> projectIds);

    @PostMapping(value = "/choerodon/v1/organizations/{organization_id}/users/agile")
    ResponseEntity<Page<UserDTO>> pagingQueryUsersOnOrganizationAgile(@PathVariable(name = "organization_id") Long id,
                                                                      @RequestParam(name = "page") int page,
                                                                      @RequestParam(name = "size") int size,
                                                                      @RequestParam(name = "id") Long userId,
                                                                      @RequestParam String email,
                                                                      @RequestParam String param,
                                                                      @RequestBody List<Long> notSelectUserIds);

    @GetMapping(value = "/choerodon/v1/organizations/{organization_id}/time_zone_work_calendars/detail")
    ResponseEntity<TimeZoneWorkCalendarRefDetailVO> queryTimeZoneWorkCalendarDetail(@PathVariable(name = "organization_id") Long organizationId,
                                                                                    @RequestParam(name = "year") Integer year);

    @GetMapping(value = "/choerodon/v1/organizations/{organization_id}/users/{user_id}/projects_simple")
    ResponseEntity<List<ProjectVO>> listProjectsByUserIdForSimple(@PathVariable("organization_id") Long organizationId,
                                                                  @PathVariable("user_id") Long userId,
                                                                  @RequestParam(required = false) String category,
                                                                  @RequestParam(required = false) Boolean enabled);

}

