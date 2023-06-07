package io.choerodon.agile.app.service.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import io.choerodon.agile.app.service.FilePathService;
import io.choerodon.agile.infra.config.AgileServiceConfigurationProperties;
import io.choerodon.agile.infra.utils.AssertUtilsForCommonException;
import io.choerodon.core.exception.CommonException;

/**
 * @author superlee
 * @since 2022-03-15
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class FilePathServiceImpl implements FilePathService {

    @Autowired
    private AgileServiceConfigurationProperties configurationProperties;

    private static final String DIVIDING_LINE = "/";

    @Override
    public String generateRelativePath(String fullPath) {
        AssertUtilsForCommonException.notEmpty(fullPath, "error.file.full.url.empty");
        final String attachmentUrl = configurationProperties.getAttachment().getUrl();
        if (!fullPath.startsWith(attachmentUrl)) {
            throw new CommonException("error.fullPath.not.match.attachmentUrl");
        }
        return fullPath.substring(attachmentUrl.length());
    }

    @Override
    public String generateFullPath(String relativePath) {
        AssertUtilsForCommonException.notEmpty(relativePath, "error.file.relativePath.empty");
        final String attachmentUrl = configurationProperties.getAttachment().getUrl();
        StringBuilder builder = new StringBuilder();
        if (!relativePath.startsWith(DIVIDING_LINE)) {
            relativePath = DIVIDING_LINE + relativePath;
        }
        builder.append(attachmentUrl).append(relativePath);
        return builder.toString();
    }
}
