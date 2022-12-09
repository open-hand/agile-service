package io.choerodon.agile.infra.feign;

import java.util.List;
import java.util.Set;
import javax.validation.Valid;

import io.swagger.annotations.ApiParam;
import org.hzero.common.HZeroService;
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
@FeignClient(value = HZeroService.Iam.NAME, fallbackFactory = BaseFallbackFactory.class)
public interface IamFeignClient {

    /**
     * 分页查询租户信息
     * @param tenantName 模糊查询条件--租户名称
     * @param tenantNum 模糊查询条件--租户名称
     * @param page 分页数
     * @param size 页面大小
     * @return Page<TenantVO>
     */
    @GetMapping({"/v1/tenants"})
    ResponseEntity<String> pagingTenants(
            @RequestParam String tenantName,
            @RequestParam String tenantNum,
            @RequestParam int page,
            @RequestParam int size
    );

    /**
     * 查询用户信息
     *
     * @param organizationId organizationId
     * @param id             id
     * @return UserDTO
     */
    @GetMapping(value = "/choerodon/v1/organizations/{organization_id}/users/{id}")
    ResponseEntity<String> query(@PathVariable(name = "organization_id") Long organizationId,
                                 @PathVariable("id") Long id);

    @PostMapping(value = "/choerodon/v1/users/ids")
    ResponseEntity<String> listUsersByIds(@RequestBody Long[] ids,
                                          @RequestParam(name = "only_enabled") Boolean onlyEnabled);

    @PostMapping(value = "/choerodon/v1/users/real_names")
    ResponseEntity<String> listUsersByRealNames(@RequestParam(name = "only_enabled") Boolean onlyEnabled,
                                                @RequestBody Set<String> realNames);



    @GetMapping(value = "/choerodon/v1/organizations/{organization_id}/users")
    ResponseEntity<String> queryUsersByOrganization(@PathVariable("organization_id") Long projectId,
                                                    @RequestParam("param") String param,
                                                    @RequestParam int page,
                                                    @RequestParam int size);

    @PostMapping(value = "/choerodon/v1/organizations/{organization_id}/users/page")
    ResponseEntity<String> pagingUsersOnOrganizationLevel(@PathVariable(name = "organization_id") Long organizationId,
                                                          @RequestParam int page,
                                                          @RequestParam int size,
                                                          @RequestBody(required = false) AgileUserVO agileUserVO);

    @GetMapping(value = "/choerodon/v1/organizations/{organization_id}")
    ResponseEntity<String> query(@PathVariable(name = "organization_id") Long id);

    @PostMapping(value = "/choerodon/v1/organizations/{organization_id}/users/agile")
    ResponseEntity<String> pagingQueryUsersOnOrganizationAgile(@PathVariable(name = "organization_id") Long id,
                                                               @RequestParam(name = "page") int page,
                                                               @RequestParam(name = "size") int size,
                                                               @RequestParam(name = "id") Long userId,
                                                               @RequestParam String email,
                                                               @RequestParam String param,
                                                               @RequestBody List<Long> notSelectUserIds);
}

