package io.choerodon.agile.infra.feign;

import java.util.Collection;
import java.util.List;
import java.util.Set;
import javax.validation.Valid;

import io.swagger.annotations.ApiParam;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;
import org.springframework.web.bind.annotation.*;

import io.choerodon.agile.api.vo.AgileUserVO;
import io.choerodon.agile.api.vo.ProjectSearchVO;
import io.choerodon.agile.api.vo.ProjectVO;
import io.choerodon.agile.api.vo.RoleAssignmentSearchVO;
import io.choerodon.agile.infra.feign.fallback.BaseFallbackFactory;

/**
 * @author dinghuang123@gmail.com
 * @since 2018/5/24
 */
@Component
@FeignClient(value = "choerodon-base", fallbackFactory = BaseFallbackFactory.class)
public interface BaseFeignClient {

    /**
     * 按照Id查询项目
     *
     * @param id 要查询的项目ID
     * @return 查询到的项目
     */
    @GetMapping(value = "/choerodon/v1/projects/{id}")
    ResponseEntity<String> queryProject(@PathVariable("id") Long id);


    /**
     * 按照Id查询项目
     *
     * @param id 要查询的项目ID
     * @return 查询到的项目
     */
    @GetMapping(value = "/choerodon/v1/projects/{id}")
    ResponseEntity<String> queryProject(@PathVariable("id") Long id,
                                        @RequestParam(value = "with_agile_info") Boolean withAgileInfo);

    @GetMapping(value = "/choerodon/v1/projects/{id}/users")
    ResponseEntity<String> listUsersByProjectId(@PathVariable("id") Long id,
                                                @RequestParam("page") int page,
                                                @RequestParam("size") int size,
                                                @RequestParam("param") String param);


    @PostMapping(value = "/choerodon/v1/projects/{id}/agile_users")
    ResponseEntity<String> agileUsers(@PathVariable("id") Long projectId,
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
    ResponseEntity<String> list(@PathVariable("id") Long id,
                                @RequestParam("param") String param);

    @PostMapping(value = "/choerodon/v1/projects/{project_id}/role_members/users/count")
    ResponseEntity<String> listRolesWithUserCountOnProjectLevel(
            @PathVariable(name = "project_id") Long sourceId,
            @RequestBody(required = false) @Valid RoleAssignmentSearchVO roleAssignmentSearchVO);

    @PostMapping(value = "/choerodon/v1/projects/{project_id}/role_members/users")
    ResponseEntity<String> pagingQueryUsersByRoleIdOnProjectLevel(
            @RequestParam(name = "page") int page,
            @RequestParam(name = "size") int size,
            @RequestParam(name = "role_id", required = false) Long roleId,
            @RequestParam Boolean doPage,
            @PathVariable(name = "project_id") Long sourceId,
            @RequestBody(required = false) @Valid RoleAssignmentSearchVO roleAssignmentSearchVO);

    @PostMapping(value = "/choerodon/v1/projects/{project_id}/role_members/users/roles")
    ResponseEntity<String> pagingQueryUsersWithProjectLevelRoles(
            @RequestParam(name = "page") int page,
            @RequestParam(name = "size") int size,
            @PathVariable(name = "project_id") Long sourceId,
            @RequestBody(required = false) @Valid RoleAssignmentSearchVO roleAssignmentSearchVO,
            @RequestParam(name = "doPage") boolean doPage);

    /**
     * 根据组织id查询所有项目
     *
     * @param organizationId organizationId
     * @return result
     */
    @GetMapping(value = "/choerodon/v1/organizations/{organization_id}/projects/all")
    ResponseEntity<String> listProjectsByOrgId(@PathVariable("organization_id") Long organizationId);

    /**
     * @param organizationId organizationId
     * @return result
     */
    @PostMapping(value = "/choerodon/v1/organizations/{organization_id}/projects/all_with_category")
    ResponseEntity<String> listWithCategoryByOrganizationIds(@PathVariable("organization_id") Long organizationId,
                                                             @RequestBody ProjectSearchVO projectSearchVO,
                                                             @RequestParam Integer page,
                                                             @RequestParam Integer size);

    /**
     * 根据组织id获取时区工作日历
     *
     * @param organizationId organizationId
     * @return result
     */
    @GetMapping(value = "/choerodon/v1/organizations/{organization_id}/time_zone_work_calendars/query_by_org_id")
    ResponseEntity<String> queryTimeZoneDetailByOrganizationId(@ApiParam(value = "组织id", required = true)
                                                               @PathVariable(name = "organization_id") Long organizationId);

    /**
     * 根据年份查询工作日历假期(包含查询年份和下一年份数据)
     *
     * @param organizationId organizationId
     * @param year           year
     * @return result
     */
    @GetMapping(value = "/choerodon/v1/organizations/{organization_id}/work_calendar_holiday_refs")
    ResponseEntity<String> queryWorkCalendarHolidayRelByYear(@ApiParam(value = "项目id", required = true)
                                                             @PathVariable(name = "organization_id") Long organizationId,
                                                             @ApiParam(value = "要查询的年份", required = true)
                                                             @RequestParam(name = "year") Integer year);

    /**
     * 根据年份查询工作日历，包含当年、去年、明年
     *
     * @param organizationId organizationId
     * @param year           year
     * @return result
     */
    @GetMapping(value = "/choerodon/v1/organizations/{organization_id}/work_calendar_holiday_refs/year_include_last_and_next")
    ResponseEntity<String> queryByYearIncludeLastAndNext(@ApiParam(value = "项目id", required = true)
                                                         @PathVariable(name = "organization_id") Long organizationId,
                                                         @ApiParam(value = "要查询的年份", required = true)
                                                         @RequestParam(name = "year") Integer year);


    @GetMapping(value = "/choerodon/v1/projects/{project_id}/role_members/users/{user_id}")
    ResponseEntity<String> getUserWithProjLevelRolesByUserId(@PathVariable(name = "project_id") Long projectId,
                                                             @PathVariable(name = "user_id") Long userId);

    @GetMapping(value = "/choerodon/v1/projects/{project_id}/owner/list")
    ResponseEntity<String> listProjectOwnerById(@PathVariable(name = "project_id") Long projectId);

    @PostMapping(value = "/choerodon/v1/projects/{project_id}/role_members/users/by_role_ids")
    ResponseEntity<String> listUsersUnderRoleByIds(@PathVariable(name = "project_id") Long projectId,
                                                   @RequestParam String roleIdString);

    @GetMapping("/choerodon/v1/users/{id}/projects/{project_id}/check_is_owner")
    ResponseEntity<String> checkIsProjectOwner(@PathVariable("id") Long id, @PathVariable("project_id") Long projectId);

    @GetMapping("choerodon/v1/users/{id}/projects")
    ResponseEntity<String> queryProjects(@PathVariable("id") Long id,
                                         @RequestParam(required = false, name = "included_disabled")
                                         boolean includedDisabled);

    @GetMapping("/choerodon/v1/organizations/{organization_id}/users/{user_id}/projects")
    ResponseEntity<String> queryOrgProjects(@PathVariable("organization_id") Long organizationId,
                                            @PathVariable("user_id") Long userId);

    @GetMapping(value = "/choerodon/v1/projects/{project_id}/roles")
    ResponseEntity<String> listProjectRoles(@PathVariable("project_id") Long projectId,
                                            @RequestParam(name = "only_select_enable") Boolean onlySelectEnable,
                                            @RequestParam(name = "role_name") String roleName);

    @GetMapping(value = "/choerodon/v1/roles/search")
    ResponseEntity<String> listOrganizationRoles(@RequestParam Integer page,
                                                 @RequestParam Integer size,
                                                 @RequestParam String name,
                                                 @RequestParam String code,
                                                 @RequestParam String roleLevel,
                                                 @RequestParam Long tenantId,
                                                 @RequestParam Boolean builtIn,
                                                 @RequestParam Boolean enabled,
                                                 @RequestParam String params);

    @GetMapping(value = "/choerodon/v1/projects/{project_id}/users")
    ResponseEntity<String> queryUsersByProject(@PathVariable("project_id") Long projectId,
                                               @RequestParam("param") String param,
                                               @RequestParam int page,
                                               @RequestParam int size);

    @PostMapping(value = "/choerodon/v1/inner/projects/list_owner")
    ResponseEntity<String> listProjectOwnerByIds(@RequestBody Set<Long> projectIds);

    @PostMapping(value = "/choerodon/v1/projects/{project_id}/agile_users_by_projects")
    ResponseEntity<String> agileUsersByProjectIds(@PathVariable(name = "project_id") Long projectId,
                                                  @RequestParam("page") int page,
                                                  @RequestParam("size") int size,
                                                  @RequestBody AgileUserVO agileUserVO);

    @PostMapping(value = "/choerodon/v1/list_roles")
    ResponseEntity<String> listRolesByIds(@RequestParam("tenantId") Long tenantId,
                                          @RequestBody List<Long> roleIds);


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
    @GetMapping(value = "/choerodon/v1/organizations/{organization_id}/projects")
    ResponseEntity<String> pagedQueryProjects(@PathVariable(name = "organization_id") Long organizationId,
                                              @RequestParam(name = "page") int page,
                                              @RequestParam(name = "size") int size,
                                              @RequestParam String name,
                                              @RequestParam String code,
                                              @RequestParam Boolean enabled,
                                              @RequestParam Boolean withAdditionInfo,
                                              @RequestParam String params);

    @PostMapping(value = "/choerodon/v1/projects/ids")
    ResponseEntity<String> queryProjectByIds(@RequestBody Set<Long> projectIds);


    @GetMapping(value = "/choerodon/v1/organizations/{organization_id}/time_zone_work_calendars/detail")
    ResponseEntity<String> queryTimeZoneWorkCalendarDetail(@PathVariable(name = "organization_id") Long organizationId,
                                                           @RequestParam(name = "year") Integer year);

    @GetMapping(value = "/choerodon/v1/organizations/{organization_id}/users/{user_id}/projects_simple")
    ResponseEntity<String> listProjectsByUserIdForSimple(@PathVariable("organization_id") Long organizationId,
                                                         @PathVariable("user_id") Long userId,
                                                         @RequestParam(required = false) String category,
                                                         @RequestParam(required = false) Boolean enabled);

    @PostMapping(value = "/choerodon/v1/organizations/{organization_id}/users/{user_id}/projects/paging_query")
    ResponseEntity<String> pagingQueryProjectsByUserId(@PathVariable("organization_id") Long organizationId,
                                                       @PathVariable("user_id") Long userId,
                                                       @RequestParam Integer page,
                                                       @RequestParam Integer size,
                                                       @RequestBody ProjectVO projectVO);

    /**
     * 查询组织下所有项目
     *
     * @param organizationId 组织ID
     * @param projectCodes   过滤条件--项目编码
     * @param enabledFlag    过滤条件--是否启用
     * @return 查询结果
     */
    @GetMapping(value = "/choerodon/v1/organizations/{organization_id}/projects/all")
    ResponseEntity<String> listProjectsByOrgId(@PathVariable("organization_id") Long organizationId,
                                               @RequestParam(required = false) Collection<String> projectCodes,
                                               @RequestParam(required = false) Boolean enabledFlag);

    @GetMapping(value = "/choerodon/v1/organizations/{organization_id}/users/{user_id}/member_of_org")
    ResponseEntity<String> memberOfOrganization(@PathVariable("organization_id") Long organizationId,
                                                @PathVariable("user_id") Long userId);
}

