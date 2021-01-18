package io.choerodon.agile.app.service;

import org.springframework.http.ResponseEntity;
import org.springframework.web.context.request.WebRequest;

import java.io.IOException;
import java.util.List;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import io.choerodon.agile.api.vo.StaticFileHeaderVO;
import io.choerodon.agile.api.vo.StaticFileRelatedVO;

/**
 * @author chihao.ran@hand-china.com
 * 2021/01/11 11:27
 */
public interface StaticFileService {

    /**
     * 上传ui/ux压缩包
     * @param projectId 项目id
     * @param issueId 关联问题id
     * @param request 含上传的资源文件
     * @return 上传的ui/ux基本信息
     */
    List<StaticFileHeaderVO> uploadStaticFile(Long projectId, Long issueId, HttpServletRequest request);

    /**
     * 重定向资源请求
     * @param projectId 项目id
     * @param fileHeaderIdStr 静态文件头id
     * @param webRequest
     * @param httpRequest
     * @param httpResponse
     * @return
     */
    ResponseEntity<byte[]> selectStaticFileResult(Long projectId, String fileHeaderIdStr, WebRequest webRequest, HttpServletRequest httpRequest, HttpServletResponse httpResponse) throws IOException;

    /**
     * 查询项目下未关联问题的静态文件头
     * @param projectId 项目id
     * @return 未关联问题的静态文件头
     */
    List<StaticFileHeaderVO> selectFileListByProject(Long projectId);

    /**
     * 查询项目下未关联问题的静态文件头
     * @param projectId 项目id
     * @param issueId 问题id
     * @return 未关联问题的静态文件头
     */
    List<StaticFileHeaderVO> selectFileListByIssue(Long projectId, Long issueId);

    /**
     * 删除静态文件列表与问题的关联关系
     * @param projectId 项目id
     * @param fileHeaderId 静态文件头id
     */
    void deleteStaticFileRelated(Long projectId, Long fileHeaderId);

    /**
     * 删除静态文件列表与问题的关联关系
     * @param projectId 项目id
     * @param fileHeaderId 静态文件头id
     */
    void deleteStaticFile(Long projectId, Long fileHeaderId);

    /**
     * 将未关联问题的静态文件关联问题
     * @param projectId 项目id
     * @param staticFileRelatedVO 关联参数
     * @return 静态文件头
     */
    List<StaticFileHeaderVO> updateStaticFileRelatedIssue(Long projectId, StaticFileRelatedVO staticFileRelatedVO);
}
