package io.choerodon.agile.app.service;

import io.choerodon.agile.infra.dto.WorkLogDTO;

/**
 * Created by HuangFuqiang@choerodon.io on 2019/9/5.
 * Email: fuqianghuang01@gmail.com
 */
public interface IWorkLogService {

    WorkLogDTO createBase(WorkLogDTO workLogDTO);

    void deleteBase(Long projectId,Long logId);
}
