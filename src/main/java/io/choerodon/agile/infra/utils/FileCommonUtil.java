package io.choerodon.agile.infra.utils;

import org.apache.commons.lang3.StringUtils;

import org.hzero.core.base.BaseConstants;

/**
 * 项目字段
 *
 * @author 汪翔 2023-05-15
 */
public class FileCommonUtil {

    public static String getFileType(String fileKey) {
        if (StringUtils.isEmpty(fileKey)) {
            return StringUtils.EMPTY;
        }
        int index = fileKey.lastIndexOf(BaseConstants.Symbol.POINT);
        if (index > -1) {
            return fileKey.substring(index + 1);
        } else {
            return StringUtils.EMPTY;
        }
    }

}
