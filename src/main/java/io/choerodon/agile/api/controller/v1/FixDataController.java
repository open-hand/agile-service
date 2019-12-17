package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.dto.MessageDetailDTO;
import io.choerodon.agile.infra.feign.NotifyFeignClient;
import io.choerodon.core.annotation.Permission;
import io.choerodon.core.enums.ResourceType;
import io.choerodon.core.iam.InitRoleCode;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;


/**
 * Created by HuangFuqiang@choerodon.io on 2018/11/13.
 * Email: fuqianghuang01@gmail.com
 */
@RestController
@RequestMapping(value = "/v1/fix_data")
public class FixDataController {

    private static final Logger LOGGER = LoggerFactory.getLogger(FixDataController.class);

    @Autowired
    private FixDataService fixDataService;

    @Autowired
    private NoticeService noticeService;

    @Autowired
    private NotifyFeignClient notifyFeignClient;


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

    @Permission(type = ResourceType.PROJECT, roles = {InitRoleCode.PROJECT_OWNER})
    @ApiOperation("【0.20 BASE】迁移agile_message_detail到框架")
    @GetMapping("/migrate_message")
    public ResponseEntity<List<MessageDetailDTO>> migrateMessageDetail(@ApiParam(value = "项目id", required = true)
                                                                       @PathVariable(name = "project_id") Long projectId) {
        return new ResponseEntity<>(noticeService.migrateMessageDetail(projectId),HttpStatus.OK);
    }

    @Permission(type = ResourceType.SITE, roles = {InitRoleCode.SITE_ADMINISTRATOR, InitRoleCode.SITE_DEVELOPER})
    @ApiOperation("【0.20 BASE】启动敏捷迁移到Base服务task")
    @GetMapping("/migration_to_base")
    public ResponseEntity migrateToBase() {
        LOGGER.info("==============================>>>>>>>> AGILE Data Migrate Start In Controller <<<<<<<<=================================");
        notifyFeignClient.checkLog("0.20.0", "agile");
        return new ResponseEntity<>(HttpStatus.OK);
    }
}
