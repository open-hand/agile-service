package io.choerodon.agile.api.controller.v1;

import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.hzero.starter.keyencrypt.core.Encrypt;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Optional;

import io.choerodon.agile.api.vo.FileOperationHistoryVO;
import io.choerodon.agile.app.service.ExcelService;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.swagger.annotation.Permission;

/**
 * Created by HuangFuqiang@choerodon.io on 2019/2/25.
 * Email: fuqianghuang01@gmail.com
 */
@RestController
@RequestMapping(value = "/v1/organizations/{organization_id}/excel")
public class ExcelOrgController {

    @Autowired
    private ExcelService excelService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询最近的上传/下载记录")
    @GetMapping(value = "/latest")
    public ResponseEntity<FileOperationHistoryVO> queryLatestRecode(@ApiParam(value = "项目id", required = true)
                                                                     @PathVariable(name = "organization_id") Long organizationId,
                                                                    @RequestParam String action) {
        return Optional.ofNullable(excelService.queryOrgLatestRecode(organizationId, action))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.ImportHistoryRecode.get"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("取消导入")
    @PutMapping(value = "/cancel")
    public ResponseEntity cancelImport(@ApiParam(value = "项目id", required = true)
                                       @PathVariable(name = "organization_id") Long organizationId,
                                       @ApiParam(value = "file history id", required = true)
                                       @RequestParam @Encrypt Long id,
                                       @ApiParam(value = "objectVersionNumber", required = true)
                                       @RequestParam Long objectVersionNumber) {
        excelService.cancelImport(0L, id, objectVersionNumber);
        return new ResponseEntity(HttpStatus.NO_CONTENT);
    }
}
