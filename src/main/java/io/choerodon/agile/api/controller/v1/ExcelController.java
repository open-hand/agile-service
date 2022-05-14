package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.api.vo.ExcelTemplateVO;
import io.choerodon.agile.api.vo.FileOperationHistoryVO;
import io.choerodon.agile.api.vo.SearchVO;
import io.choerodon.agile.app.assembler.BoardAssembler;
import io.choerodon.agile.app.service.ExcelService;

import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.agile.infra.utils.EncryptionUtils;
import io.choerodon.agile.infra.utils.MultipartFileUtil;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.mybatis.pagehelper.annotation.SortDefault;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.mybatis.pagehelper.domain.Sort;
import io.choerodon.swagger.annotation.Permission;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.DetailsHelper;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.hzero.starter.keyencrypt.core.Encrypt;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;
import org.springframework.web.multipart.MultipartFile;
import springfox.documentation.annotations.ApiIgnore;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.Optional;

/**
 * Created by HuangFuqiang@choerodon.io on 2019/2/25.
 * Email: fuqianghuang01@gmail.com
 */
@RestController
@RequestMapping(value = "/v1/projects/{project_id}/excel")
public class ExcelController {

    @Autowired
    private ExcelService excelService;
    @Autowired
    private BoardAssembler boardAssembler;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("下载issue导入模版")
    @PostMapping(value = "/download")
    public void download(@ApiParam(value = "项目id", required = true)
                         @PathVariable(name = "project_id") Long projectId,
                         @ApiParam(value = "组织id", required = true)
                         @RequestParam Long organizationId,
                         @ApiParam(value = "响应体")
                         HttpServletResponse response,
                         @ApiParam(value = "模版下载筛选参数")
                         @RequestBody ExcelTemplateVO excelTemplateVO) {
        excelService.download(projectId, organizationId, response, excelTemplateVO);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("导入issue")
    @PostMapping(value = "/import")
    public ResponseEntity batchImport(@ApiParam(value = "项目id", required = true)
                                      @PathVariable(name = "project_id") Long projectId,
                                      @ApiParam(value = "组织id", required = true)
                                      @RequestParam Long organizationId,
                                      @ApiParam(value = "导入文件", required = true)
                                      @RequestParam("file") MultipartFile file) {
        Long userId = DetailsHelper.getUserDetails().getUserId();
        excelService.batchImport(projectId, organizationId, userId, MultipartFileUtil.getInputStreamFromMultipartFile(file), (ServletRequestAttributes) RequestContextHolder.currentRequestAttributes());
        return new ResponseEntity(HttpStatus.NO_CONTENT);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("取消导入")
    @PutMapping(value = "/cancel")
    public ResponseEntity cancelImport(@ApiParam(value = "项目id", required = true)
                                       @PathVariable(name = "project_id") Long projectId,
                                       @ApiParam(value = "file history id", required = true)
                                       @RequestParam @Encrypt Long id,
                                       @ApiParam(value = "objectVersionNumber", required = true)
                                       @RequestParam Long objectVersionNumber) {
        excelService.cancelImport(projectId, id, objectVersionNumber);
        return new ResponseEntity(HttpStatus.NO_CONTENT);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询最近的上传/下载记录")
    @GetMapping(value = "/latest")
    public ResponseEntity<FileOperationHistoryVO> queryLatestRecode(@ApiParam(value = "项目id", required = true)
                                                                     @PathVariable(name = "project_id") Long projectId,
                                                                    @ApiParam(value = "历史记录行为")
                                                                    @RequestParam String action) {
        return Optional.ofNullable(excelService.queryLatestRecode(projectId, action))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.ImportHistoryRecode.get"));
    }

    @ResponseBody
    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("导出issue列表")
    @PostMapping(value = "/export")
    public void exportIssues(@ApiIgnore
                             @ApiParam(value = "分页信息", required = true)
                             @SortDefault(value = "issueNum", direction = Sort.Direction.DESC)
                                     PageRequest pageRequest,
                             @ApiParam(value = "项目id", required = true)
                             @PathVariable(name = "project_id") Long projectId,
                             @ApiParam(value = "组织id", required = true)
                             @RequestParam Long organizationId,
                             @ApiParam(value = "查询参数", required = true)
                             @RequestBody(required = false) SearchVO searchVO,
                             @ApiParam(value = "http请求")
                             HttpServletRequest request,
                             @ApiParam(value = "http响应")
                             HttpServletResponse response) {
        EncryptionUtils.decryptSearchVO(searchVO);
        //处理未匹配的筛选
        boardAssembler.handleOtherArgs(searchVO);
        excelService.asyncExportIssues(projectId, searchVO, request, response, organizationId, pageRequest.getSort(), (ServletRequestAttributes) RequestContextHolder.currentRequestAttributes());
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("下载自定义字段导入模版")
    @GetMapping(value = "/object_scheme_field/download")
    public void downloadObjectSchemeField(@ApiParam(value = "项目id", required = true)
                                          @PathVariable(name = "project_id") Long projectId,
                                          @ApiParam(value = "组织id", required = true)
                                          @RequestParam Long organizationId,
                                          @ApiParam(value = "http响应")
                                                  HttpServletResponse response) {
        excelService.downloadObjectSchemeField(organizationId, projectId, response);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("导入自定义字段")
    @PostMapping(value = "/object_scheme_field/import")
    public ResponseEntity<Void> batchImportObjectSchemeField(@ApiParam(value = "项目id", required = true)
                                            @PathVariable(name = "project_id") Long projectId,
                                            @ApiParam(value = "导入文件", required = true)
                                            @RequestParam("file") MultipartFile file) {
        excelService.batchImportObjectSchemeField(
                ConvertUtil.getOrganizationId(projectId), projectId,
                MultipartFileUtil.getInputStreamFromMultipartFile(file),
                RequestContextHolder.getRequestAttributes());
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }
}
