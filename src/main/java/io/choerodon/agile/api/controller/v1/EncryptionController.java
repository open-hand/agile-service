package io.choerodon.agile.api.controller.v1;

import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.hzero.starter.keyencrypt.core.Encrypt;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.mapper.IssueMapper;
import io.choerodon.agile.infra.utils.EncryptionUtils;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.swagger.annotation.Permission;


/**
 * @author zhaotianxin
 * @date 2020-07-29 17:47
 */
@RestController
@RequestMapping(value = "/v1/projects/{project_id}")
public class EncryptionController {

    @Autowired
    private IssueMapper issueMapper;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("加密")
    @GetMapping("/encrypt")
    public ResponseEntity<String> encrypt(@ApiParam(value = "项目id", required = true)
                                          @PathVariable(name = "project_id") Long projectId,
                                          @ApiParam(value = "issueId")
                                          @RequestParam(required = false) Long issueId,
                                          @ApiParam(value = "需要加密的id")
                                          @RequestParam(required = false) Long id) {
        if (issueId != null) {
            id = issueId;
        }
        return ResponseEntity.ok(EncryptionUtils.encrypt(id));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("解密issueId")
    @GetMapping("/decrypt")
    public ResponseEntity<Long> decrypt(
            @ApiParam(value = "项目id", required = true)
            @PathVariable(name = "project_id") Long projectId,
            @ApiParam(value = "issue id")
            @RequestParam @Encrypt Long issueId) {
        IssueDTO issueDTO = new IssueDTO();
        issueDTO.setProjectId(projectId);
        issueDTO.setIssueId(issueId);
        if (ObjectUtils.isEmpty(issueMapper.selectOne(issueDTO))) {
            throw new CommonException("error.issue.notFound");
        }
        return ResponseEntity.ok(issueId);
    }
}
