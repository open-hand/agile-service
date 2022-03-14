package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.infra.enums.FileUploadBucket;
import org.hzero.boot.file.FileClient;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.util.List;

import io.choerodon.agile.app.service.StaticFileDealService;
import io.choerodon.agile.infra.annotation.DataLog;
import io.choerodon.agile.infra.dto.StaticFileHeaderDTO;
import io.choerodon.agile.infra.dto.StaticFileIssueRelDTO;
import io.choerodon.agile.infra.dto.StaticFileLineDTO;
import io.choerodon.agile.infra.dto.StaticFileOperationHistoryDTO;
import io.choerodon.agile.infra.mapper.StaticFileHeaderMapper;
import io.choerodon.agile.infra.mapper.StaticFileIssueRelMapper;
import io.choerodon.agile.infra.mapper.StaticFileLineMapper;
import io.choerodon.agile.infra.mapper.StaticFileOperationHistoryMapper;
import io.choerodon.core.exception.CommonException;

/**
 * @author chihao.ran@hand-china.com
 * 2021/01/11 11:28
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class StaticFileDealServiceImpl implements StaticFileDealService {

    private static final String INSERT_ERROR = "error.StaticFile.create";
    private static final String DELETE_FAILED = "delete_failed";
    private static final String FAILED = "failed";
    private static final String SUCCESS = "success";

    @Autowired
    private StaticFileHeaderMapper staticFileHeaderMapper;
    @Autowired
    private StaticFileIssueRelMapper staticFileIssueRelMapper;
    @Autowired
    private StaticFileLineMapper staticFileLineMapper;
    @Autowired
    private StaticFileOperationHistoryMapper staticFileOperationHistoryMapper;
    @Autowired
    private FileClient fileClient;

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
    @Async
    public void deleteBase(StaticFileIssueRelDTO relRecord, StaticFileHeaderDTO staticFileHeader, List<String> fileUrls, StaticFileOperationHistoryDTO staticFileDeleteHistory) {
        StaticFileLineDTO lineRecord = new StaticFileLineDTO();
        lineRecord.setHeaderId(staticFileHeader.getId());
        relRecord.setStaticFileId(staticFileHeader.getId());
        if (!CollectionUtils.isEmpty(fileUrls)) {
            try {
                deleteUrlFile(staticFileHeader.getOrganizationId(), fileUrls);
            } catch (Exception e) {
                staticFileDeleteHistory.setErrorMessage(e.getMessage());
                staticFileDeleteHistory.setStatus(FAILED);
                staticFileOperationHistoryMapper.updateByPrimaryKeySelective(staticFileDeleteHistory);
                staticFileHeaderMapper.updateFileStatus(staticFileHeader.getId(), DELETE_FAILED);
                return;
            }
        }
        staticFileDeleteHistory.setStatus(SUCCESS);
        staticFileOperationHistoryMapper.updateByPrimaryKeySelective(staticFileDeleteHistory);

        staticFileLineMapper.delete(lineRecord);
        staticFileIssueRelMapper.delete(relRecord);
        if (staticFileHeaderMapper.deleteByPrimaryKey(staticFileHeader.getId()) != 1) {
            throw new CommonException("error.staticFileHeader.delete");
        }
    }

    private void deleteUrlFile(Long organizationId, List<String> fileUrls) {
        int lastIndex;
        int size = 80;
        for (int i = 0; i <= (fileUrls.size() / size); i++) {
            if (i == (fileUrls.size() / size)) {
                lastIndex = fileUrls.size();
            } else {
                lastIndex = size * (i + 1);
            }
            if (!fileUrls.subList(size * i, lastIndex).isEmpty()) {
                fileClient.deleteFileByUrl(organizationId, FileUploadBucket.AGILE_BUCKET.bucket(), fileUrls.subList(size * i, lastIndex));
            }
        }
    }
}
