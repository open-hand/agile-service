package io.choerodon.agile.infra.utils;

import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.io.InputStream;

import io.choerodon.core.exception.CommonException;

/**
 * @author chihao.ran@hand-china.com
 * 2021/05/20 20:07
 */
public class MultipartFileUtil {

    private MultipartFileUtil() {

    }

    /**
     * 获取MultipartFile的InputStream
     * @param file 文件
     * @return MultipartFile的InputStream
     */
    public static InputStream getInputStreamFromMultipartFile(MultipartFile file) {
        try {
            return file.getInputStream();
        } catch (IOException e) {
            throw new CommonException("error.multipartFile.to.inputStream", e);
        }
    }
}
