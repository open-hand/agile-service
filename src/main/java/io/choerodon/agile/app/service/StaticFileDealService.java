package io.choerodon.agile.app.service;

import io.choerodon.agile.infra.dto.StaticFileHeaderDTO;

/**
 * @author chihao.ran@hand-china.com
 * 2021/01/11 11:27
 */
public interface StaticFileDealService {

    /**
     * 创建静态文件头记录
     * @param staticFileHeaderDTO 静态文件头记录
     * @return 静态文件头记录
     */
    StaticFileHeaderDTO createBase(StaticFileHeaderDTO staticFileHeaderDTO);

    /**
     * 删除静态文件头记录
     * @param fileHeaderId 静态文件头id
     * @return 成功
     */
    Boolean deleteBase(Long fileHeaderId);
}
