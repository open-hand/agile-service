package io.choerodon.agile.infra.feign.fallback;

import io.choerodon.agile.infra.feign.CustomFileRemoteService;
import io.choerodon.core.exception.CommonException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;
import java.util.Map;

/**
 * @author superlee
 * @since 2020-05-22
 */
@Component
public class CustomFileRemoteServiceFallback implements CustomFileRemoteService {

    private static final Logger logger = LoggerFactory.getLogger(CustomFileRemoteServiceFallback.class);

    @Override
    public ResponseEntity<String> deleteFileByUrl(Long organizationId, String bucketName, List<String> urls) {
        logger.error("Delete file failed,organizationId = {}, bucketName = {}.", organizationId, bucketName);
        throw new CommonException("File service is not available, please check", new Object[0]);
    }

    @Override
    public ResponseEntity<String> fragmentCombineBlock(Long organizationId, String guid, String fileName, Map<String, String> args) {
        logger.error("Combine file failed,organizationId = {}, fileName = {}, guid = {}.", organizationId, fileName, guid);
        throw new CommonException("File service is not available, please check");
    }
}
