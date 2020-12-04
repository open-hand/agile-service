package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.TableHeaderVO;

import java.util.List;

/**
 * @author superlee
 * @since 2020-11-25
 */
public interface TableHeaderService {
    /**
     * 根据code获取ui界面的表头数据
     *
     * @param code
     * @return
     */
    List<TableHeaderVO> listByCode(String code);
}
