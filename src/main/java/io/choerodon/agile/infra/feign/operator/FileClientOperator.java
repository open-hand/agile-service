package io.choerodon.agile.infra.feign.operator;

import com.fasterxml.jackson.core.type.TypeReference;
import io.choerodon.agile.api.vo.FileVO;
import io.choerodon.agile.infra.feign.FileFeignClient;
import org.hzero.core.util.ResponseUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * 项目字段
 *
 * @author 汪翔 2023-05-15
 */
@Component
public class FileClientOperator {
    @Autowired
    private FileFeignClient fileFeignClient;

    public List<FileVO> queryFileDTOByIds(Long organizationId, List<String> fileKeys) {
        return ResponseUtils.getResponse(fileFeignClient.queryFileDTOByIds(organizationId, fileKeys), new TypeReference<List<FileVO>>() {
        });
    }


}