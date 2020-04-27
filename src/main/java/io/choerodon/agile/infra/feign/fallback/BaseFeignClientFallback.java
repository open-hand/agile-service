package io.choerodon.agile.infra.feign.fallback;

import com.github.pagehelper.PageInfo;
import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.infra.dto.TimeZoneWorkCalendarDTO;
import io.choerodon.agile.infra.dto.UserDTO;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.core.exception.CommonException;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;

import javax.validation.Valid;
import java.util.List;
import java.util.Set;

/**
 * @author dinghuang123@gmail.com
 * @since 2018/5/24
 */
@Component
public class BaseFeignClientFallback implements BaseFeignClient {

    private static final String QUERY_ERROR = "error.UserFeign.query";
    private static final String BATCH_QUERY_ERROR = "error.UserFeign.queryList";

    @Override
    public ResponseEntity<UserDTO> query(Long organizationId, Long id) {
        throw new CommonException(QUERY_ERROR);
    }

    @Override
    public ResponseEntity<List<UserDTO>> listUsersByIds(Long[] ids, Boolean onlyEnabled) {
        throw new CommonException(BATCH_QUERY_ERROR);
    }

    @Override
    public ResponseEntity<ProjectVO> queryProject(Long id) {
        throw new CommonException(QUERY_ERROR);
    }

    @Override
    public ResponseEntity<PageInfo<UserDTO>> listUsersByProjectId(Long id, int page, int size) {
        throw new CommonException(QUERY_ERROR);
    }

    @Override
    public ResponseEntity<PageInfo<UserDTO>> agileUsers(Long projectId, int page, int size, String param, Set<Long> userIds) {
        throw new CommonException(QUERY_ERROR);
    }

    @Override
    public ResponseEntity<PageInfo<UserVO>> list(Long id, String param) {
        throw new CommonException(QUERY_ERROR);
    }

    @Override
    public ResponseEntity<List<RoleVO>> listRolesWithUserCountOnProjectLevel(Long sourceId, RoleAssignmentSearchVO roleAssignmentSearchVO) {
        throw new CommonException(QUERY_ERROR);
    }

    @Override
    public ResponseEntity<PageInfo<UserVO>> pagingQueryUsersByRoleIdOnProjectLevel(int page, int size, Long roleId, Long sourceId, RoleAssignmentSearchVO roleAssignmentSearchVO) {
        throw new CommonException("error.users.get");
    }

    @Override
    public ResponseEntity<List<ProjectRelationshipVO>> getProjUnderGroup(Long orgId, Long id, Boolean onlySelectEnable) {
        throw new CommonException("error.projUnderGroup.get");
    }

    @Override
    public ResponseEntity<ProjectVO> getGroupInfoByEnableProject(Long organizationId, Long projectId) {
        throw new CommonException("error.groupInfo.get");
    }

    @Override
    public ResponseEntity<PageInfo<UserWithRoleVO>> pagingQueryUsersWithProjectLevelRoles(int page, int size, Long sourceId, @Valid RoleAssignmentSearchVO roleAssignmentSearchVO, boolean doPage) {
        throw new CommonException("error.usersWithRoles.get");
    }

    @Override
    public ResponseEntity<List<ProjectVO>> listProjectsByOrgId(Long organizationId) {
        throw new CommonException("error.iamServiceFeignFallback.listProjectsByOrgId");
    }


    @Override
    public ResponseEntity<TimeZoneWorkCalendarDTO> queryTimeZoneDetailByOrganizationId(Long organizationId) {
        throw new CommonException("error.iamServiceFeignFallback.queryTimeZoneDetailByOrganizationId");
    }

    @Override
    public ResponseEntity<List<WorkCalendarHolidayRefVO>> queryWorkCalendarHolidayRelByYear(Long organizationId, Integer year) {
        throw new CommonException("error.iamServiceFeignFallback.queryWorkCalendarHolidayRelByYear");
    }

    @Override
    public ResponseEntity<List<WorkCalendarHolidayRefVO>> queryByYearIncludeLastAndNext(Long organizationId, Integer year) {
        throw new CommonException("error.iamServiceFeignFallback.queryByYearIncludeLastAndNext");
    }

    @Override
    public ResponseEntity<List<RoleVO>> getUserWithProjLevelRolesByUserId(Long projectId, Long userId) {
        throw new CommonException("error.iamServiceFeignFallback.getUserWithProjLevelRolesByUserId");
    }
}
