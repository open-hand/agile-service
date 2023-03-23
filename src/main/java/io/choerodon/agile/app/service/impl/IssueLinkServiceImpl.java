package io.choerodon.agile.app.service.impl;

import org.hzero.starter.keyencrypt.core.EncryptContext;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.web.context.request.RequestContextHolder;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import io.choerodon.agile.api.validator.IssueLinkValidator;
import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.business.IssueListFieldKVVO;
import io.choerodon.agile.app.assembler.IssueLinkAssembler;
import io.choerodon.agile.app.service.IssueLinkService;
import io.choerodon.agile.app.service.IssueOperateService;
import io.choerodon.agile.app.service.IssueService;
import io.choerodon.agile.infra.dto.IssueLinkDTO;
import io.choerodon.agile.infra.dto.LinkIssueStatusLinkageDTO;
import io.choerodon.agile.infra.dto.business.IssueConvertDTO;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.enums.SchemeApplyType;
import io.choerodon.agile.infra.feign.operator.RemoteIamOperator;
import io.choerodon.agile.infra.mapper.IssueLinkMapper;
import io.choerodon.agile.infra.mapper.IssueMapper;
import io.choerodon.agile.infra.mapper.IssueTypeMapper;
import io.choerodon.agile.infra.mapper.LinkIssueStatusLinkageMapper;
import io.choerodon.agile.infra.utils.BaseFieldUtil;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.CommonException;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;

/**
 * @author dinghuang123@gmail.com
 * @since 2018/6/14
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class IssueLinkServiceImpl implements IssueLinkService {

    private static final String INSERT_ERROR = "error.IssueLink.create";
    private static final String ISSUE_TYPE_ID = "issueTypeId";

    @Autowired
    private IssueLinkMapper issueLinkMapper;
    @Autowired
    private IssueLinkValidator issueLinkValidator;
    @Autowired
    private IssueLinkAssembler issueLinkAssembler;
    @Autowired
    private IssueService issueService;
    @Autowired
    private IssueTypeMapper issueTypeMapper;
    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private LinkIssueStatusLinkageMapper linkIssueStatusLinkageMapper;
    @Autowired
    private IssueMapper issueMapper;
    @Autowired
    private IssueOperateService issueOperateService;

    @Autowired
    private RemoteIamOperator remoteIamOperator;

    @Override
    public IssueLinkResponseVO createIssueLinkList(List<IssueLinkCreateVO> issueLinkCreateVOList, Long issueId, Long projectId) {
        List<IssueLinkDTO> issueLinkDTOList = issueLinkAssembler.toTargetList(issueLinkCreateVOList, IssueLinkDTO.class);
        issueLinkDTOList.forEach(issueLinkDTO -> {
            issueLinkDTO.setProjectId(projectId);
            issueLinkValidator.verifyCreateData(issueLinkDTO);
            if (Boolean.TRUE.equals(issueLinkValidator.checkUniqueLink(issueLinkDTO))) {
                create(issueLinkDTO);
                BaseFieldUtil.updateIssueLastUpdateInfoForIssueLink(issueLinkDTO.getProjectId(), issueLinkDTO);
            }
        });
        // 创建链接时候触发关联问题联动
        IssueDTO issueDTO = issueMapper.selectByPrimaryKey(issueId);
        issueOperateService.updateIssueStatusLinkage(projectId, issueId, issueDTO, SchemeApplyType.AGILE, EncryptContext.encryptType().name(), RequestContextHolder.currentRequestAttributes());
        IssueLinkResponseVO response = new IssueLinkResponseVO();
        response.setIssueLinks(listIssueLinkByIssueId(issueId, projectId, false));
        return response;
    }


    @Override
    public void deleteIssueLink(Long issueLinkId) {
        IssueLinkDTO issueLinkDTO = issueLinkMapper.selectByPrimaryKey(issueLinkId);
        BaseFieldUtil.updateIssueLastUpdateInfoForIssueLink(issueLinkDTO.getProjectId(), issueLinkDTO);
        delete(issueLinkId);
    }

    @Override
    public List<IssueLinkVO> listIssueLinkByIssueId(Long issueId, Long projectId, Boolean noIssueTest) {
        List<IssueLinkVO> issueLinkVOS = issueLinkAssembler.issueLinkDTOToVO(projectId,
                issueLinkMapper.queryIssueLinkByIssueId(new HashSet<>(Arrays.asList(issueId)), new HashSet<>(Arrays.asList(projectId)), noIssueTest));

        Set<Long> projectIds = issueLinkVOS.stream().map(IssueLinkVO::getLinkedIssueProjectId).collect(Collectors.toSet());
        projectIds.remove(projectId);
        Map<Long, ProjectVO> projectVOMap = remoteIamOperator.queryProjectByIds(projectIds).stream().collect(Collectors.toMap(ProjectVO::getId, Function.identity()));

        issueLinkVOS.forEach(issueLinkVO -> {
            if (projectVOMap.get(issueLinkVO.getLinkedIssueProjectId()) != null) {
                issueLinkVO.setLinkedProjectVO(projectVOMap.get(issueLinkVO.getLinkedIssueProjectId()));
            }
        });
        return issueLinkVOS;
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
        return modelMapper.map(issueLinkMapper.select(query), new TypeToken<List<IssueLinkDTO>>() {
        }.getType());
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
            return modelMapper.map(issueLinkDTOList, new TypeToken<List<IssueLinkFixVO>>() {
            }.getType());
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
    public Page<IssueListFieldKVVO> listUnLinkIssue(Long issueId, Long projectId, Long targetProjectId, SearchVO searchVO, PageRequest pageRequest, Long organizationId) {
        if (searchVO == null) {
            searchVO = new SearchVO();
        }
        if (searchVO.getOtherArgs() == null) {
            searchVO.setOtherArgs(new HashMap<>(1));
        }
        if (searchVO.getSearchArgs() == null) {
            searchVO.setSearchArgs(new HashMap<>(1));
        }
        if (searchVO.getAdvancedSearchArgs() == null) {
            searchVO.setAdvancedSearchArgs(new HashMap<>(1));
        }

        if (!Objects.equals(projectId, targetProjectId) && targetProjectId != null) {
            remoteIamOperator.checkTargetProjectPermission(projectId, targetProjectId, true);
            projectId = targetProjectId;
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
        searchVO.getSearchArgs().put("tree", false);
        List<Long> issueTypeIds = searchVO.getAdvancedSearchArgs().get(ISSUE_TYPE_ID) == null ? new ArrayList<>() : (List<Long>) searchVO.getAdvancedSearchArgs().get(ISSUE_TYPE_ID);
        if (CollectionUtils.isEmpty(issueTypeIds)) {
            IssueTypeSearchVO issueTypeSearchVO = new IssueTypeSearchVO();
            issueTypeSearchVO.setTypeCodes(Stream.of("story", "task", "bug", "stage", "milestone", "activity").collect(Collectors.toList()));
            List<IssueTypeVO> issueTypes = issueTypeMapper.selectByOptions(organizationId, projectId, issueTypeSearchVO);
            issueTypeIds = issueTypes.stream().map(IssueTypeVO::getId).collect(Collectors.toList());
            searchVO.getAdvancedSearchArgs().put(ISSUE_TYPE_ID, issueTypeIds);
        }
        return issueService.listIssueWithSub(projectId, searchVO, pageRequest, organizationId);
    }

    @Override
    public List<Long> checkLinkIssueCycle(Long projectId, Long issueId, Long linkTypeId, List<Long> linkIssueIds) {
        if (CollectionUtils.isEmpty(linkIssueIds)) {
            return new ArrayList<>();
        }
        IssueDTO issueDTO = issueMapper.selectByPrimaryKey(issueId);
        List<IssueDTO> issueDTOS = issueMapper.listIssueInfoByIssueIds(projectId, linkIssueIds, null);
        if (CollectionUtils.isEmpty(issueDTOS)) {
            return new ArrayList<>();
        }
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        List<IssueLinkChangeVO> issueLinkChangeVOS = issueLinkMapper.issueLinkChangeByProjectId(projectId);
        if (CollectionUtils.isEmpty(issueLinkChangeVOS)) {
            issueLinkChangeVOS = new ArrayList<>();
        }
        Map<Long, IssueDTO> linkIssueMap = issueDTOS.stream().collect(Collectors.toMap(IssueDTO::getIssueId, Function.identity()));
        List<Long> illegalLinkIssueIds = new ArrayList<>();
        LinkIssueStatusLinkageDTO linkIssueStatusLinkageDTO = new LinkIssueStatusLinkageDTO(null, null, projectId, organizationId);
        linkIssueStatusLinkageDTO.setLinkTypeId(linkTypeId);
        List<LinkIssueStatusLinkageDTO> linkageDTOS = linkIssueStatusLinkageMapper.select(linkIssueStatusLinkageDTO);
        Map<Long, List<LinkIssueStatusLinkageDTO>> linkageGroupMap = new HashMap<>();
        if (!CollectionUtils.isEmpty(linkageDTOS)) {
            linkageGroupMap.putAll(linkageDTOS.stream().collect(Collectors.groupingBy(LinkIssueStatusLinkageDTO::getIssueTypeId)));
        }
        for (Long linkIssueId : linkIssueIds) {
            Map<Long, List<Long>> influenceMap = new HashMap<>();
            IssueDTO linkIssue = linkIssueMap.getOrDefault(linkIssueId, null);
            List<LinkIssueStatusLinkageDTO> linkages = linkageGroupMap.getOrDefault(issueDTO.getIssueTypeId(), new ArrayList<>());
            // 构造issue和关联issue的关系
            List<IssueLinkChangeVO> list = new ArrayList<>();
            Long linkIssueStatusId = handlerIssueLinkChangeVOS(issueDTO, linkIssue, linkages, list);
            List<LinkIssueStatusLinkageDTO> linkIssueStatusLinkageDTOS = linkageGroupMap.getOrDefault(linkIssue.getIssueTypeId(), new ArrayList<>());
            handlerIssueLinkChangeVOS(linkIssue, issueDTO, linkIssueStatusLinkageDTOS, list);
            list.addAll(issueLinkChangeVOS);
            Map<Long, List<IssueLinkChangeVO>> issueLinkChangeGroup = list.stream().collect(Collectors.groupingBy(IssueLinkChangeVO::getIssueId));
            InfluenceIssueVO influenceIssueVO = new InfluenceIssueVO();

            issueService.handlerInfluenceMap(influenceMap, linkIssueId, linkIssueStatusId, issueLinkChangeGroup, null, influenceIssueVO, false);
            // 判断是否出现环
            List<Long> statusIds = influenceMap.getOrDefault(linkIssueId, new ArrayList<>());
            if (statusIds.contains(issueDTO.getStatusId())) {
                illegalLinkIssueIds.add(linkIssueId);
            }
        }
        return illegalLinkIssueIds;
    }

    private Long handlerIssueLinkChangeVOS(IssueDTO issueDTO, IssueDTO linkIssue, List<LinkIssueStatusLinkageDTO> linkages, List<IssueLinkChangeVO> list) {
        Long statusId = null;
        for (LinkIssueStatusLinkageDTO linkageDTO : linkages) {
            Boolean issueStatusEqual = Objects.equals(issueDTO.getStatusId(), linkIssue.getStatusId());
            Boolean issueTypeEqual = Objects.equals(linkageDTO.getLinkIssueTypeId(), linkIssue.getIssueTypeId());
            if (issueStatusEqual && issueTypeEqual) {
                IssueLinkChangeVO issueLinkChangeVO = new IssueLinkChangeVO();
                issueLinkChangeVO.setIssueId(issueDTO.getIssueId());
                issueLinkChangeVO.setStatusId(linkageDTO.getStatusId());
                issueLinkChangeVO.setLinkedIssueId(linkIssue.getIssueId());
                issueLinkChangeVO.setLinkIssueStatusId(linkageDTO.getLinkIssueStatusId());
                list.add(issueLinkChangeVO);
                statusId = linkageDTO.getLinkIssueStatusId();
            }
        }
        return statusId;
    }
}
