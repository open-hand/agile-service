package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.IssueLinkCreateVO;
import io.choerodon.agile.api.vo.IssueLinkFixVO;
import io.choerodon.agile.api.vo.IssueLinkVO;
import io.choerodon.agile.api.validator.IssueLinkValidator;
import io.choerodon.agile.api.vo.SearchVO;
import io.choerodon.agile.api.vo.business.IssueListFieldKVVO;
import io.choerodon.agile.app.assembler.IssueLinkAssembler;
import io.choerodon.agile.app.service.IssueLinkService;
import io.choerodon.agile.app.service.IssueService;
import io.choerodon.agile.infra.dto.business.IssueConvertDTO;
import io.choerodon.agile.infra.dto.IssueLinkDTO;
import io.choerodon.agile.infra.mapper.IssueLinkMapper;
import io.choerodon.agile.infra.utils.BaseFieldUtil;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.CommonException;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;

import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.util.*;

/**
 * @author dinghuang123@gmail.com
 * @since 2018/6/14
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class IssueLinkServiceImpl implements IssueLinkService {

    private static final String INSERT_ERROR = "error.IssueLink.create";

    @Autowired
    private IssueLinkMapper issueLinkMapper;
    @Autowired
    private IssueLinkValidator issueLinkValidator;
    @Autowired
    private IssueLinkAssembler issueLinkAssembler;
    @Autowired
    private IssueService issueService;
    @Autowired
    private ModelMapper modelMapper;

    @Override
    public List<IssueLinkVO> createIssueLinkList(List<IssueLinkCreateVO> issueLinkCreateVOList, Long issueId, Long projectId) {
        List<IssueLinkDTO> issueLinkDTOList = issueLinkAssembler.toTargetList(issueLinkCreateVOList, IssueLinkDTO.class);
        issueLinkDTOList.forEach(issueLinkDTO -> {
            issueLinkDTO.setProjectId(projectId);
            issueLinkValidator.verifyCreateData(issueLinkDTO);
            if (Boolean.TRUE.equals(issueLinkValidator.checkUniqueLink(issueLinkDTO))) {
                create(issueLinkDTO);
                BaseFieldUtil.updateIssueLastUpdateInfoForIssueLink(issueLinkDTO.getProjectId(), issueLinkDTO);
            }
        });
        return listIssueLinkByIssueId(issueId, projectId, false);
    }


    @Override
    public void deleteIssueLink(Long issueLinkId) {
        IssueLinkDTO issueLinkDTO = issueLinkMapper.selectByPrimaryKey(issueLinkId);
        BaseFieldUtil.updateIssueLastUpdateInfoForIssueLink(issueLinkDTO.getProjectId(), issueLinkDTO);
        delete(issueLinkId);
    }

    @Override
    public List<IssueLinkVO> listIssueLinkByIssueId(Long issueId, Long projectId, Boolean noIssueTest) {
        return issueLinkAssembler.issueLinkDTOToVO(projectId,
                issueLinkMapper.queryIssueLinkByIssueId(new HashSet<>(Arrays.asList(issueId)), new HashSet<>(Arrays.asList(projectId)), noIssueTest));
    }

    @Override
    public List<IssueLinkVO> listIssueLinkByBatch(Long projectId, List<Long> issueIds) {
        return issueLinkAssembler.issueLinkDTOToVO(projectId, issueLinkMapper.listIssueLinkByBatch(projectId, issueIds));
    }

    @Override
    public List<IssueLinkDTO> create(IssueLinkDTO issueLinkDTO) {
        if (issueLinkMapper.insert(issueLinkDTO) != 1) {
            throw new CommonException(INSERT_ERROR);
        }
        IssueLinkDTO query = new IssueLinkDTO();
        query.setIssueId(issueLinkDTO.getIssueId());
        return modelMapper.map(issueLinkMapper.select(query), new TypeToken<List<IssueLinkDTO>>(){}.getType());
    }

    @Override
    public int deleteByIssueId(Long issueId) {
        BaseFieldUtil.updateIssueLastUpdateInfoForALLIssueLink(issueLinkMapper, issueId);
        return issueLinkMapper.deleteByIssueId(issueId);
    }

    @Override
    public int delete(Long issueLinkId) {
        return issueLinkMapper.deleteByPrimaryKey(issueLinkId);
    }

    @Override
    public List<IssueLinkFixVO> listIssueLinkByIssuedIds(Long projectId) {
        List<IssueLinkDTO> issueLinkDTOList = issueLinkMapper.listIssueLinkByIssueIds(projectId);
        if (issueLinkDTOList != null && !issueLinkDTOList.isEmpty()) {
            return modelMapper.map(issueLinkDTOList, new TypeToken<List<IssueLinkFixVO>>() {}.getType());
        } else {
            return new ArrayList<>();
        }
    }

    @Override
    public void deleteIssueLinkByIssueId(IssueConvertDTO issueConvertDTO, List<IssueLinkDTO> issueLinkDTOS) {
        BaseFieldUtil.updateIssueLastUpdateInfoForIssueLinks(issueConvertDTO, issueLinkDTOS);
        issueLinkMapper.deleteByIssueId(issueConvertDTO.getIssueId());
    }

    @Override
    public Page<IssueListFieldKVVO> listUnLinkIssue(Long issueId, Long projectId, SearchVO searchVO, PageRequest pageRequest, Long organizationId) {
        if (searchVO == null) {
            searchVO = new SearchVO();
        }
        if (searchVO.getOtherArgs() == null) {
            searchVO.setOtherArgs(new HashMap<>(1));
        }

        Set<Long> issueIds = new HashSet<>();
        issueIds.add(issueId);
        List<IssueLinkDTO> issueLinks = issueLinkMapper.queryIssueLinkByIssueId(new HashSet<>(Arrays.asList(issueId)), new HashSet<>(Arrays.asList(projectId)), false);
        if (!CollectionUtils.isEmpty(issueLinks)) {
            issueLinks.forEach(issueLink -> {
                issueIds.add(issueLink.getIssueId());
                issueIds.add(issueLink.getLinkedIssueId());
            });
        }
        searchVO.getOtherArgs().put("excludeIssueIds", issueIds);
        return issueService.listIssueWithSub(projectId, searchVO, pageRequest, organizationId);
    }
}
