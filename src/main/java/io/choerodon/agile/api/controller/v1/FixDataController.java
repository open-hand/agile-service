package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.app.service.*;
import io.choerodon.base.annotation.Permission;
import io.choerodon.base.enums.ResourceType;
import io.choerodon.core.iam.InitRoleCode;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;


/**
 * Created by HuangFuqiang@choerodon.io on 2018/11/13.
 * Email: fuqianghuang01@gmail.com
 */
@RestController
@RequestMapping(value = "/v1/fix_data")
public class FixDataController {

    @Autowired
    private FixDataService fixDataService;


    @Permission(type = ResourceType.SITE, roles = {InitRoleCode.SITE_ADMINISTRATOR, InitRoleCode.SITE_DEVELOPER})
    @ApiOperation("修复0.19创建项目产生的脏数据【全部】")
    @GetMapping(value = "/fix_create_project_0.19")
    public ResponseEntity fixCreateProject() {
        fixDataService.fixCreateProject();
        return new ResponseEntity<>(HttpStatus.OK);
    }

    @Permission(type = ResourceType.SITE, roles = {InitRoleCode.SITE_ADMINISTRATOR, InitRoleCode.SITE_DEVELOPER})
    @ApiOperation("修复0.19创建项目产生的脏数据【单个】")
    @GetMapping(value = "/fix_create_project_0.19_single")
    public ResponseEntity fixCreateProjectSingle(@RequestParam("projectId") Long projectId) {
        fixDataService.fixCreateProjectSingle(projectId);
        return new ResponseEntity<>(HttpStatus.OK);
    }

}
