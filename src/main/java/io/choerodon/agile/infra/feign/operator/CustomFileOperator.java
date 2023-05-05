package io.choerodon.agile.infra.feign.operator;

import java.util.List;

import com.fasterxml.jackson.core.type.TypeReference;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import io.choerodon.agile.infra.feign.CustomFileFeignClient;
import io.choerodon.agile.infra.feign.vo.FileVO;

import org.hzero.core.util.ResponseUtils;

/**
 * @author superlee
 * @since 2023/5/4
 */
@Component
public class CustomFileOperator {

    @Autowired
    private CustomFileFeignClient customFileFeignClient;

    /**
     * 根据fileKey查询文件，fileKey为全路径去除文件服务域名和bucket和/
     * fullUrl = https://minio.dev.choerodon.com.cn/agile-service/1128/CHOERODON-MINIO/e2a2d61cddb441f8a1b299c3b13d8cda@uat.xml
     * fileKey = 1128/CHOERODON-MINIO/e2a2d61cddb441f8a1b299c3b13d8cda@uat.xml
     *
     * @param organizationId
     * @param fileKeys
     * @return
     */
    public List<FileVO> getFileByFileKey(Long organizationId, List<String> fileKeys) {
        return ResponseUtils.getResponse(customFileFeignClient.queryFileByFileKeys(organizationId, fileKeys), new TypeReference<List<FileVO>>() {
        });
    }
}
