package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.TableHeaderVO;
import io.choerodon.agile.app.service.TableHeaderService;
import io.choerodon.agile.infra.enums.header.TableHeader;
import io.choerodon.agile.infra.enums.header.TableHeaderCode;
import io.choerodon.core.exception.CommonException;
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
        if (!TableHeaderCode.notExisted(code)) {
            throw new CommonException("error.illegal.table.header.code." + code);
        }
        return TableHeader.get(code);
    }
}
