package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.app.service.InnerFeignService;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.swagger.annotation.Permission;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * @author superlee
 * @since 2021-04-15
 */
@RestController
@RequestMapping(value = "/v1/inner")
public class InnerFeignController {

    @Autowired
    private InnerFeignService innerFeignService;

    @Permission(permissionWithin = true)
    @PostMapping(value = "/issues/by_ids")
    @ApiOperation(value = "根据issueIds查询issue")
    public ResponseEntity<List<IssueDTO>> listIssueByIds(@RequestBody List<Long> issueIds) {
        return new ResponseEntity<>(innerFeignService.listIssueByIds(issueIds), HttpStatus.OK);
    }
}
