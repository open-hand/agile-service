package io.choerodon.agile.infra.feign;


import io.choerodon.agile.infra.feign.fallback.FileFeignClientFallbackFactory;
import io.swagger.annotations.ApiOperation;
import org.hzero.common.HZeroService;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;

import java.util.List;


/**
 * 项目字段
 *
 * @author 汪翔 2023-05-15
 */
@FeignClient(value = HZeroService.File.NAME, fallbackFactory = FileFeignClientFallbackFactory.class)
public interface FileFeignClient {

    @ApiOperation(value = "根据文件的keys集合 查询文件数据")
    @PostMapping("/choerodon/v1/{organization_id}/file/list")
    ResponseEntity<String> queryFileDTOByIds(@PathVariable("organization_id") Long organizationId,
                                                   @RequestBody List<String> fileKeys);



}

