package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.api.vo.WorkHoursLogVO;
import io.choerodon.agile.api.vo.WorkHoursSearchVO;
import io.choerodon.agile.app.service.WorkHoursService;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.mybatis.pagehelper.annotation.SortDefault;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.mybatis.pagehelper.domain.Sort;
import io.choerodon.swagger.annotation.Permission;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Arrays;
import java.util.Optional;

/**
 * @author zhaotianxin
 * @date 2021-10-15 14:15
 */
@RestController
@RequestMapping(value = "/v1/organizations/{organization_id}/work_hours")
public class WorkHoursOrgController {

    @Autowired
    private WorkHoursService workHoursService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询工时日志")
    @PostMapping(value = "/work_hours_log")
    public ResponseEntity<Page<WorkHoursLogVO>> pageWorkHoursLogByOrgId(@ApiParam(value = "组织id", required = true)
                                                                             @PathVariable(name = "organization_id") Long organizationId,
                                                                             @SortDefault(value = "creationDate", direction = Sort.Direction.DESC)
                                                                             PageRequest pageRequest,
                                                                             @RequestBody WorkHoursSearchVO workHoursSearchVO) {
        return Optional.ofNullable(workHoursService.pageWorkHoursLogByOrgId(organizationId, pageRequest, workHoursSearchVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.workCalendar.get"));
    }
}
