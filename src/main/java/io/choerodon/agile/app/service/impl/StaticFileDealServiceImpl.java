package io.choerodon.agile.app.service.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import io.choerodon.agile.app.service.StaticFileDealService;
import io.choerodon.agile.infra.annotation.DataLog;
import io.choerodon.agile.infra.dto.StaticFileHeaderDTO;
import io.choerodon.agile.infra.dto.StaticFileIssueRelDTO;
import io.choerodon.agile.infra.mapper.StaticFileHeaderMapper;
import io.choerodon.agile.infra.mapper.StaticFileIssueRelMapper;
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
    @Autowired
    private StaticFileIssueRelMapper staticFileIssueRelMapper;

    @Override
    @DataLog(type = "createStaticFile")
    public StaticFileHeaderDTO createBase(StaticFileHeaderDTO staticFileHeaderDTO, StaticFileIssueRelDTO staticFileIssueRelDTO) {
        if (staticFileHeaderMapper.insert(staticFileHeaderDTO) != 1) {
            throw new CommonException(INSERT_ERROR);
        }
        staticFileIssueRelDTO.setStaticFileId(staticFileHeaderDTO.getId());
        staticFileIssueRelMapper.insert(staticFileIssueRelDTO);
        return staticFileHeaderMapper.selectByPrimaryKey(staticFileHeaderDTO.getId());
    }

    @Override
    @DataLog(type = "deleteStaticFileRelated")
    public void deleteStaticFileRelated(StaticFileHeaderDTO staticFileHeaderDTO, StaticFileIssueRelDTO staticFileIssueRelDTO) {
        staticFileIssueRelDTO.setStaticFileId(staticFileHeaderDTO.getId());
        staticFileIssueRelMapper.delete(staticFileIssueRelDTO);
    }

    @Override
    @DataLog(type = "createStaticFileRelated")
    public void updateStaticFileRelatedIssue(StaticFileIssueRelDTO newRelDTO, StaticFileHeaderDTO staticFileHeaderDTO) {
        newRelDTO.setStaticFileId(staticFileHeaderDTO.getId());
        staticFileIssueRelMapper.insert(newRelDTO);
    }

    @Override
    @DataLog(type = "deleteStaticFile")
    public void deleteRel(StaticFileIssueRelDTO relRecord, StaticFileHeaderDTO staticFileHeader) {
        relRecord.setStaticFileId(staticFileHeader.getId());
        staticFileIssueRelMapper.delete(relRecord);
    }
}
