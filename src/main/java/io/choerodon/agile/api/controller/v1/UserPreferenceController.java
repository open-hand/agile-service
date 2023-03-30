package io.choerodon.agile.api.controller.v1;

import java.util.List;
import java.util.Map;

import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.util.Assert;
import org.springframework.web.bind.annotation.*;

import io.choerodon.agile.app.service.UserPreferenceService;
import io.choerodon.agile.domain.repository.UserPreferenceRepository;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.core.oauth.CustomUserDetails;
import io.choerodon.core.oauth.DetailsHelper;
import io.choerodon.swagger.annotation.Permission;

import org.hzero.core.base.BaseConstants;
import org.hzero.core.base.BaseController;
import org.hzero.core.util.Results;

/**
 * 用户偏好设置 管理 API -- 项目层
 *
 * @author gaokuo.dai@zknow.com 2023-03-30 19:21:29
 * @since 2.4
 */
@RestController("userPreferenceController.v1" )
@RequestMapping("/v1/projects/{project_id}/user-preferences" )
public class UserPreferenceController extends BaseController {

    @Autowired
    private UserPreferenceRepository userPreferenceRepository;
    @Autowired
    private UserPreferenceService userPreferenceService;


    @ApiOperation(value = "查询用户偏好设置")
    @Permission(level = ResourceLevel.ORGANIZATION)
    @GetMapping("/detail")
    public ResponseEntity<Map<String, Object>> userPreferencesDetail(@PathVariable("project_id") Long projectId) {
        final CustomUserDetails userDetails = DetailsHelper.getUserDetails();
        Assert.notNull(userDetails, BaseConstants.ErrorCode.NOT_LOGIN);
        final Long userId = userDetails.getUserId();
        final Long organizationId = ConvertUtil.getOrganizationId(projectId);
        Map<String, Object> result = userPreferenceRepository.findUserPreferencesDetail(organizationId, projectId, userId);
        return Results.success(result);
    }

    @ApiOperation(value = "查询用户偏好设置--部分设置")
    @Permission(level = ResourceLevel.ORGANIZATION)
    @GetMapping("/detail/partial")
    public ResponseEntity<Map<String, Object>> userPreferencesPartialDetail(
            @PathVariable("project_id") Long projectId,
            @ApiParam(value = "偏好设置键") @RequestParam List<String> preferenceKeys
    ) {
        final CustomUserDetails userDetails = DetailsHelper.getUserDetails();
        Assert.notNull(userDetails, BaseConstants.ErrorCode.NOT_LOGIN);
        final Long userId = userDetails.getUserId();
        final Long organizationId = ConvertUtil.getOrganizationId(projectId);
        Map<String, Object> result = userPreferenceRepository.findUserPreferencesPartialDetail(organizationId, projectId, userId, preferenceKeys);
        return Results.success(result);
    }

    @ApiOperation(value = "保存用户偏好设置")
    @Permission(level = ResourceLevel.ORGANIZATION)
    @PutMapping
    public ResponseEntity<Map<String, Object>> save(@PathVariable("project_id") Long projectId, @RequestBody Map<String, Object> preferenceToSave) {
        final CustomUserDetails userDetails = DetailsHelper.getUserDetails();
        Assert.notNull(userDetails, BaseConstants.ErrorCode.NOT_LOGIN);
        final Long userId = userDetails.getUserId();
        final Long organizationId = ConvertUtil.getOrganizationId(projectId);
        userPreferenceService.save(organizationId, projectId, userId, preferenceToSave);
        return Results.success(userPreferenceRepository.findUserPreferencesPartialDetail(organizationId, projectId, userId, preferenceToSave.keySet()));
    }

}
