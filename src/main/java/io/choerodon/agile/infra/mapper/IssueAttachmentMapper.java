package io.choerodon.agile.infra.mapper;

import java.util.List;
import io.choerodon.mybatis.common.BaseMapper;
import io.choerodon.agile.infra.dto.*;
import org.apache.ibatis.annotations.Param;


/**
 * Created by HuangFuqiang@choerodon.io on 2018/5/16.
 * Email: fuqianghuang01@gmail.com
 */
public interface IssueAttachmentMapper extends BaseMapper<IssueAttachmentDTO> {

    List<TestCaseAttachmentDTO> listAttachmentDTO();

    void updateProject(@Param("projectId") Long projectId, @Param("issueId") Long issueId, @Param("targetProjectId") Long targetProjectId);
}