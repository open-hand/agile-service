package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.DataLogCreateVO;
import io.choerodon.agile.api.vo.DataLogFixVO;
import io.choerodon.agile.api.vo.DataLogQueryVO;
import io.choerodon.agile.api.vo.business.AllDataLogVO;
import io.choerodon.agile.api.vo.business.DataLogVO;
import io.choerodon.agile.infra.dto.DataLogDTO;
import io.choerodon.agile.infra.dto.DataLogStatusChangeDTO;
import io.choerodon.core.domain.Page;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;

import java.util.List;
import java.util.Set;

/**
 * Created by HuangFuqiang@choerodon.io on 2018/6/14.
 * Email: fuqianghuang01@gmail.com
 */
public interface DataLogService {

    DataLogVO createDataLog(Long projectId, DataLogCreateVO createVO);

    List<DataLogVO> listByIssueId(Long projectId, Long issueId);

    DataLogDTO create(DataLogDTO dataLogDTO);

    void delete(DataLogDTO dataLogDTO);

    /**
     * 根据id批量删除错误数据
     *
     * @param dataLogIds dataLogIds
     */
    void batchDeleteErrorDataLog(Set<Long> dataLogIds);

    /**
     * 更新脏数据
     *
     * @param dataLogStatusChangeDTOS dataLogStatusChangeDTOS
     */
    void batchUpdateErrorDataLog(Set<DataLogStatusChangeDTO> dataLogStatusChangeDTOS);

    List<DataLogFixVO> queryListByProjectId(Long projectId);

    /**
     * 查询项目层操作动态，即问题下操作历史
     * @param projectId 项目id
     * @param dataLogQueryVO 查询参数
     * @param pageRequest 分页参数
     * @return 项目层操作动态
     */
    Page<AllDataLogVO> listAllDataLogByProjectId(Long projectId, DataLogQueryVO dataLogQueryVO, PageRequest pageRequest);
}
