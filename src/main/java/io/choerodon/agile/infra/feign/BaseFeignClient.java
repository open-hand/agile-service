package io.choerodon.agile.infra.feign;

import io.choerodon.core.domain.Page;
import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.infra.dto.TimeZoneWorkCalendarDTO;
import io.choerodon.agile.infra.dto.UserDTO;
import io.choerodon.agile.infra.feign.fallback.BaseFeignClientFallback;
import io.swagger.annotations.ApiParam;
import org.hzero.common.HZeroService;
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
@FeignClient(value = HZeroService.Iam.NAME, fallback = BaseFeignClientFallback.class)
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

    @GetMapping(value = "/choerodon/v1/projects/{id}/users")
    ResponseEntity<Page<UserDTO>> listUsersByProjectId(@PathVariable("id") Long id,
                                                       @RequestParam("page") int page,
                                                       @RequestParam("size") int size);


    @PostMapping(value = "/choerodon/v1/projects/{id}/agile_users")
    ResponseEntity<Page<UserDTO>> agileUsers(@PathVariable("id") Long projectId,
                                                 @RequestParam("page") int page,
                                                 @RequestParam("size") int size,
                                                 @RequestParam("param") String param,
                                                 @RequestBody Set<Long> userIds);

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
            @RequestParam(name = "role_id") Long roleId,
            @PathVariable(name = "project_id") Long sourceId,
            @RequestBody(required = false) @Valid RoleAssignmentSearchVO roleAssignmentSearchVO);

    @GetMapping(value = "/choerodon/v1/organizations/{organization_id}/project_relations/{parent_id}")
    ResponseEntity<List<ProjectRelationshipVO>> getProjUnderGroup(@PathVariable(name = "organization_id") Long orgId,
                                                                  @PathVariable(name = "parent_id") Long id,
                                                                  @RequestParam(name = "only_select_enable") Boolean onlySelectEnable);

    @GetMapping(value = "/choerodon/v1/organizations/{organization_id}/projects/{project_id}/program")
    ResponseEntity<ProjectVO> getGroupInfoByEnableProject(@PathVariable(name = "organization_id") Long organizationId,
                                                          @PathVariable(name = "project_id") Long projectId);

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
}

