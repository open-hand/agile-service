package io.choerodon.agile.app.service.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import io.choerodon.agile.app.service.StaticFileDealService;
import io.choerodon.agile.infra.annotation.DataLog;
import io.choerodon.agile.infra.dto.StaticFileHeaderDTO;
import io.choerodon.agile.infra.mapper.StaticFileHeaderMapper;
import io.choerodon.core.exception.CommonException;

/**
 * @author chihao.ran@hand-china.com
 * 2021/01/11 11:28
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class StaticFileDealServiceImpl implements StaticFileDealService {

    private static final String INSERT_ERROR = "error.StaticFile.create";

    @Autowired
    private StaticFileHeaderMapper staticFileHeaderMapper;

    @Override
    @DataLog(type = "createStaticFile")
    public StaticFileHeaderDTO createBase(StaticFileHeaderDTO staticFileHeaderDTO) {
        if (staticFileHeaderMapper.insert(staticFileHeaderDTO) != 1) {
            throw new CommonException(INSERT_ERROR);
        }
        return staticFileHeaderMapper.selectByPrimaryKey(staticFileHeaderDTO.getId());
    }

    @Override
    @DataLog(type = "deleteStaticFile")
    public Boolean deleteBase(Long fileHeaderId) {
        if (staticFileHeaderMapper.deleteByPrimaryKey(fileHeaderId) != 1) {
            throw new CommonException("error.staticFileHeader.delete");
        }
        return true;
    }

}
