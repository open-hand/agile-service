package io.choerodon.agile.app.service;

import java.util.List;

import io.choerodon.agile.api.vo.TableHeaderVO;

/**
 * @author superlee
 * @since 2020-11-25
 */
public interface TableHeaderService {
    /**
     * 根据code获取ui界面的表头数据
     *
     * @param code code
     * @return result
     */
    List<TableHeaderVO> listByCode(String code);
}
