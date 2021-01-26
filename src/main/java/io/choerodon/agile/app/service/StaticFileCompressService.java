package io.choerodon.agile.app.service;

import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.util.List;

import io.choerodon.agile.infra.dto.StaticFileCompressDTO;
import io.choerodon.agile.infra.dto.StaticFileOperationHistoryDTO;

/**
 * @author chihao.ran@hand-china.com
 * 2021/01/11 11:27
 */
public interface StaticFileCompressService {

    /**
     * 校验压缩文件
     * @param files 要校验的压缩文件
     */
    void validFileType(List<MultipartFile> files);

    /**
     * 解压静态文件压缩包
     * @param staticFileCompressList 传入的要解压的参数
     * @param projectId 项目id
     * @param organizationId 租户id
     * @param staticFileCompressHistoryList 解压操作信息
     * @param issueId 问题id
     */
    void unCompress(List<StaticFileCompressDTO> staticFileCompressList, Long projectId, Long organizationId, List<StaticFileOperationHistoryDTO> staticFileCompressHistoryList, Long issueId);

    /**
     * 获取index文件相对路径
     * @param multipartFile multipartFile
     * @return 相对路径
     * @throws IOException IOException
     */
    String getIndexPrefixPath(MultipartFile multipartFile) throws IOException;
}
