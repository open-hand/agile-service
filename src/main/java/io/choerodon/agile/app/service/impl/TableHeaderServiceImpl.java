package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.TableHeaderVO;
import io.choerodon.agile.app.service.TableHeaderService;
import io.choerodon.agile.infra.enums.header.TableHeader;
import io.choerodon.agile.infra.enums.header.TableHeaderCode;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * @author superlee
 * @since 2020-11-25
 */
@Service
public class TableHeaderServiceImpl implements TableHeaderService {

    @Override
    public List<TableHeaderVO> listByCode(String code) {
        TableHeaderCode.notExisted(code, true);
        return TableHeader.get(code);
    }
}
