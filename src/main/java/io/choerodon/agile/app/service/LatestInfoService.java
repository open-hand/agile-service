package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.DataLogQueryVO;
import io.choerodon.agile.api.vo.business.AllDataLogVO;
import io.choerodon.core.domain.Page;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;

/**
 * @author zhaotianxin
 * @date 2020-10-12 10:35
 */
public interface LatestInfoService {

    /**
     * 查询项目层操作动态，即问题下操作历史
     * @param projectId 项目id
     * @param dataLogQueryVO 查询参数
     * @param pageRequest 分页参数
     * @return 项目层操作动态
     */
    Page<AllDataLogVO> listLatestOperationInfoByProjectId(Long projectId, DataLogQueryVO dataLogQueryVO, PageRequest pageRequest);
}
