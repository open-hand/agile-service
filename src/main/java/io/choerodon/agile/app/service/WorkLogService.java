package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.WorkLogVO;
import io.choerodon.agile.infra.dto.WorkLogDTO;
import io.choerodon.agile.infra.dto.business.IssueConvertDTO;

import java.util.List;

/**
 * Created by HuangFuqiang@choerodon.io on 2018/5/18.
 * Email: fuqianghuang01@gmail.com
 */
public interface WorkLogService {

    WorkLogVO createWorkLog(Long projectId, WorkLogVO workLogVO);

    WorkLogVO updateWorkLog(Long projectId, Long logId, WorkLogVO workLogVO);

    void deleteWorkLog(Long projectId, Long logId);

    WorkLogVO queryWorkLogById(Long projectId, Long logId);

    List<WorkLogVO> queryWorkLogListByIssueId(Long projectId, Long issueId);

    WorkLogDTO updateBase(WorkLogDTO workLogDTO);

    void updateRemainingTime(IssueConvertDTO issueConvertDTO,
                             Long projectId,
                             Long issueId,
                             List<String> fieldList);

}
