package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.dto.IssueLinkTypeDTO;
import io.choerodon.agile.infra.dto.LinkIssueStatusLinkageDTO;
import io.choerodon.agile.infra.dto.StatusMachineTransformDTO;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.dto.business.IssueDetailDTO;
import io.choerodon.agile.infra.enums.SchemeApplyType;
import io.choerodon.agile.infra.mapper.IssueLinkTypeMapper;
import io.choerodon.agile.infra.mapper.IssueMapper;
import io.choerodon.agile.infra.mapper.LinkIssueStatusLinkageMapper;
import io.choerodon.agile.infra.mapper.StatusMachineTransformMapper;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.core.exception.CommonException;
import org.hzero.mybatis.domian.Condition;
import org.hzero.mybatis.util.Sqls;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * @author zhaotianxin
 * @date 2021-06-09 11:17
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class LinkIssueStatusLinkageServiceImpl implements LinkIssueStatusLinkageService {

    @Autowired
    private LinkIssueStatusLinkageMapper linkIssueStatusLinkageMapper;

    @Autowired
    private ModelMapper modelMapper;

    @Autowired
    private ProjectConfigService projectConfigService;

    @Autowired
    private IssueLinkTypeMapper issueLinkTypeMapper;

    @Autowired
    private IssueMapper issueMapper;

    @Autowired
    private IssueLinkService issueLinkService;

    @Autowired
    private StatusMachineTransformMapper statusMachineTransformMapper;

    @Autowired
    private IssueService issueService;

    @Override
    public List<LinkIssueStatusLinkageVO> createOrUpdate(Long projectId, Long organizationId, Long issueTypeId, Long statusId, List<LinkIssueStatusLinkageVO> linkIssueStatusLinkageVOS) {
        LinkIssueStatusLinkageDTO linkIssueStatusLinkageDTO = new LinkIssueStatusLinkageDTO(issueTypeId, statusId, projectId, organizationId);
        List<LinkIssueStatusLinkageDTO> linkageDTOS = linkIssueStatusLinkageMapper.select(linkIssueStatusLinkageDTO);
        if (!CollectionUtils.isEmpty(linkageDTOS)) {
            linkageDTOS.forEach(v -> linkIssueStatusLinkageMapper.deleteByPrimaryKey(v.getId()));
        }
        if (!CollectionUtils.isEmpty(linkIssueStatusLinkageVOS)) {
            linkIssueStatusLinkageVOS.forEach(v -> {
                v.setStatusId(statusId);
                checkData(v, projectId, organizationId, issueTypeId);
                LinkIssueStatusLinkageDTO linkageDTO = modelMapper.map(v, LinkIssueStatusLinkageDTO.class);
                linkageDTO.setIssueTypeId(issueTypeId);
                linkageDTO.setStatusId(statusId);
                linkageDTO.setProjectId(projectId);
                linkageDTO.setOrganizationId(organizationId);
                baseInsert(linkageDTO);
            });
        }
        return listByIssueTypeAndStatusId(projectId, organizationId, issueTypeId, statusId);
    }

    private void checkData(LinkIssueStatusLinkageVO statusLinkageVO, Long projectId, Long organizationId, Long issueTypeId) {
        LinkIssueStatusLinkageDTO linkage = new LinkIssueStatusLinkageDTO(issueTypeId, statusLinkageVO.getStatusId(), projectId, organizationId);
        linkage.setLinkTypeId(statusLinkageVO.getLinkTypeId());
        linkage.setLinkIssueTypeId(statusLinkageVO.getLinkIssueTypeId());
        linkage.setLinkIssueStatusId(statusLinkageVO.getLinkIssueStatusId());
        List<LinkIssueStatusLinkageDTO> statusLinkageDTOS = linkIssueStatusLinkageMapper.select(linkage);
        if (!CollectionUtils.isEmpty(statusLinkageDTOS)) {
            throw new CommonException("error.link.issue.status.repeat");
        }
    }

    private void baseInsert(LinkIssueStatusLinkageDTO linkageDTO) {
        if (linkIssueStatusLinkageMapper.insertSelective(linkageDTO) != 1) {
            throw new CommonException("error.link.issue.status.linkage.insert");
        }
    }

    @Override
    public List<LinkIssueStatusLinkageVO> listByIssueTypeAndStatusId(Long projectId, Long organizationId, Long issueTypeId, Long statusId) {
        LinkIssueStatusLinkageDTO linkIssueStatusLinkageDTO = new LinkIssueStatusLinkageDTO(issueTypeId, statusId, projectId, organizationId);
        List<LinkIssueStatusLinkageDTO> linkageDTOS = linkIssueStatusLinkageMapper.select(linkIssueStatusLinkageDTO);
        if (CollectionUtils.isEmpty(linkageDTOS)) {
            return new ArrayList<>();
        }
        return handlerLinkIssueStatusLinkageVO(projectId, issueTypeId, linkageDTOS);
    }

    private List<LinkIssueStatusLinkageVO> handlerLinkIssueStatusLinkageVO(Long projectId, Long issueTypeId,List<LinkIssueStatusLinkageDTO> linkageDTOS) {
        // 获取项目的状态
        String applyType = projectConfigService.getApplyType(projectId, issueTypeId);

        List<StatusVO> statusVOS = projectConfigService.queryStatusByProjectId(projectId, applyType);
        Map<Long, StatusVO> statusMap = new HashMap<>();
        if (!CollectionUtils.isEmpty(statusVOS)) {
            statusMap.putAll(statusVOS.stream().collect(Collectors.toMap(StatusVO::getId, Function.identity())));
        }
        // 获取项目的问题类型
        List<IssueTypeVO> issueTypeVOS = projectConfigService.queryIssueTypesByProjectId(projectId, applyType, false);
        Map<Long, IssueTypeVO> typeVOMap = new HashMap<>();
        if (!CollectionUtils.isEmpty(statusVOS)) {
            typeVOMap.putAll(issueTypeVOS.stream().collect(Collectors.toMap(IssueTypeVO::getId, Function.identity())));
        }

        List<IssueLinkTypeDTO> issueLinkTypeDTOS = issueLinkTypeMapper.queryIssueLinkTypeByProjectId(projectId, null, null, null);
        Map<Long, IssueLinkTypeVO> issueLinkTypeVOMap = new HashMap<>();
        if (!CollectionUtils.isEmpty(issueLinkTypeDTOS)) {
            List<IssueLinkTypeVO> issueLinkTypeVOS = modelMapper.map(issueLinkTypeDTOS, new TypeToken<List<IssueLinkTypeVO>>() {
            }.getType());
            issueLinkTypeVOMap.putAll(issueLinkTypeVOS.stream().collect(Collectors.toMap(IssueLinkTypeVO::getLinkTypeId, Function.identity())));
        }
        List<LinkIssueStatusLinkageVO> linkIssueStatusLinkageVOS = new ArrayList<>();
        for (LinkIssueStatusLinkageDTO linkageDTO : linkageDTOS) {
            IssueLinkTypeVO linkTypeVO = issueLinkTypeVOMap.getOrDefault(linkageDTO.getLinkTypeId(), null);
            IssueTypeVO issueTypeVO = typeVOMap.getOrDefault(linkageDTO.getLinkIssueTypeId(), null);
            StatusVO statusVO = statusMap.getOrDefault(linkageDTO.getLinkIssueStatusId(), null);
            if (ObjectUtils.isEmpty(linkTypeVO) || ObjectUtils.isEmpty(issueTypeVO) || ObjectUtils.isEmpty(statusVO)) {
                continue;
            }
            LinkIssueStatusLinkageVO linkageVO = modelMapper.map(linkageDTO, LinkIssueStatusLinkageVO.class);
            linkageVO.setLinkIssueType(issueTypeVO);
            linkageVO.setLinkIssueStatus(statusVO);
            linkageVO.setLinkTypeVO(linkTypeVO);
            linkIssueStatusLinkageVOS.add(linkageVO);
        }
        return linkIssueStatusLinkageVOS;
    }

    @Override
    public Map<Long, List<LinkIssueStatusLinkageVO>> listByIssueTypeAndStatusIds(Long projectId, Long organizationId, Long issueTypeId, List<Long> statusIds) {
        Sqls existCondition = Sqls.custom().andEqualTo("projectId", projectId).andEqualTo("issueTypeId", issueTypeId).andIn("statusId", statusIds);
        List<LinkIssueStatusLinkageDTO> linkageDTOS = linkIssueStatusLinkageMapper
                .selectByCondition(Condition.builder(LinkIssueStatusLinkageDTO.class).andWhere(existCondition).build());
        if (CollectionUtils.isEmpty(linkageDTOS)) {
            return new HashMap<>();
        }
        List<LinkIssueStatusLinkageVO> linkIssueStatusLinkageVOS = handlerLinkIssueStatusLinkageVO(projectId, issueTypeId,linkageDTOS);
        return linkIssueStatusLinkageVOS.stream().collect(Collectors.groupingBy(LinkIssueStatusLinkageVO::getStatusId));
    }

    @Override
    public void updateLinkIssueStatus(Long projectId,
                                      Long issueId,
                                      String applyType,
                                      Set<Long> influenceIssueIds) {
        IssueDetailDTO issueDetailDTO = issueMapper.queryIssueDetail(projectId, issueId);
        if (ObjectUtils.isEmpty(issueDetailDTO)) {
            return;
        }
        List<IssueLinkVO> linkVOS = issueLinkService.listIssueLinkByIssueId(issueId, projectId, false);
        if (CollectionUtils.isEmpty(linkVOS)) {
            return;
        }
        ProjectVO projectVO = ConvertUtil.queryProject(projectId);
        Long organizationId = projectVO.getOrganizationId();
        // 查询当前状态是否会配置关联问题联动
        LinkIssueStatusLinkageDTO linkIssueStatusLinkageDTO = new LinkIssueStatusLinkageDTO(issueDetailDTO.getIssueTypeId(), issueDetailDTO.getStatusId(), projectId, organizationId);
        List<LinkIssueStatusLinkageDTO> linkageDTOS = linkIssueStatusLinkageMapper.select(linkIssueStatusLinkageDTO);
        if (CollectionUtils.isEmpty(linkageDTOS)) {
            return;
        }
        List<StatusVO> statusVOS = projectConfigService.queryStatusByProjectId(projectId, applyType);
        List<Long> allStatusId = new ArrayList<>();
        if (!CollectionUtils.isEmpty(statusVOS)) {
            allStatusId.addAll(statusVOS.stream().map(StatusVO::getId).collect(Collectors.toList()));
        }
        Map<Long, Map<Long, Long>> map = linkageDTOS.stream().collect(Collectors.groupingBy(LinkIssueStatusLinkageDTO::getLinkTypeId, Collectors.toMap(LinkIssueStatusLinkageDTO::getLinkIssueTypeId, LinkIssueStatusLinkageDTO::getLinkIssueStatusId)));
        Map<Long, Map<Long, Long>> issueChangeMap = new HashMap<>();
        List<Long> issueIds = new ArrayList<>();
        for (IssueLinkVO linkVO : linkVOS) {
            Long issueTypeId = linkVO.getIssueTypeVO().getId();
            Map<Long, Long> issueTypeMap = map.getOrDefault(linkVO.getLinkTypeId(), new HashMap<>());
            Long statusId = issueTypeMap.getOrDefault(linkVO.getIssueTypeVO().getId(), null);
            if (!ObjectUtils.isEmpty(statusId) && allStatusId.contains(statusId) && !ObjectUtils.isEmpty(linkVO.getLinkedIssueId())) {
                Map<Long, Long> issueMap = issueChangeMap.getOrDefault(issueTypeId, new HashMap<>());
                Long linkIssue = Objects.equals(issueId, linkVO.getLinkedIssueId()) ? linkVO.getIssueId() : linkVO.getLinkedIssueId();
                issueMap.put(linkIssue , statusId);
                issueChangeMap.put(issueTypeId, issueMap);
                issueIds.add(linkIssue);
            }
        }

        if (!CollectionUtils.isEmpty(issueChangeMap) && !CollectionUtils.isEmpty(issueIds)) {
            influenceIssueIds.addAll(changeStatus(projectId, organizationId, applyType, issueChangeMap, issueIds));
        }
    }

    @Override
    public void deleteByStatusId(Long projectId, Long organizationId, Long statusId, Long issueTypeId) {
        LinkIssueStatusLinkageDTO linkIssueStatusLinkageDTO = new LinkIssueStatusLinkageDTO(projectId, organizationId);
        linkIssueStatusLinkageDTO.setStatusId(statusId);
        linkIssueStatusLinkageDTO.setIssueTypeId(issueTypeId);
        linkIssueStatusLinkageMapper.delete(linkIssueStatusLinkageDTO);

        LinkIssueStatusLinkageDTO linkIssueStatusLinkage = new LinkIssueStatusLinkageDTO(projectId, organizationId);
        linkIssueStatusLinkageDTO.setLinkIssueStatusId(statusId);
        linkIssueStatusLinkageDTO.setLinkIssueTypeId(issueTypeId);
        linkIssueStatusLinkageMapper.delete(linkIssueStatusLinkage);
    }

    @Override
    public List<StatusVO> queryStatus(Long projectId, Long organizationId, LinkIssueStatusLinkageVO linkageVO) {
        if (ObjectUtils.isEmpty(linkageVO.getIssueTypeId()) || ObjectUtils.isEmpty(linkageVO.getLinkIssueTypeId())) {
            throw new CommonException("error.link.issue.filter.illegal");
        }
        String applyType = projectConfigService.getApplyType(projectId, linkageVO.getIssueTypeId());
        LinkIssueStatusLinkageDTO linkIssueStatusLinkageDTO = new LinkIssueStatusLinkageDTO(projectId, organizationId);
        linkIssueStatusLinkageDTO.setIssueTypeId(linkageVO.getIssueTypeId());
        linkIssueStatusLinkageDTO.setStatusId(linkageVO.getStatusId());
        linkIssueStatusLinkageDTO.setLinkTypeId(linkageVO.getLinkTypeId());
        linkIssueStatusLinkageDTO.setLinkIssueTypeId(linkageVO.getLinkIssueTypeId());
        List<LinkIssueStatusLinkageDTO> dtos = linkIssueStatusLinkageMapper.select(linkIssueStatusLinkageDTO);
        List<StatusAndTransformVO> statusAndTransformVOS = projectConfigService.statusTransformList(projectId, linkageVO.getLinkIssueTypeId(), applyType);
        List<StatusVO> statusVOS = modelMapper.map(statusAndTransformVOS, new TypeToken<List<StatusVO>>() {
        }.getType());
        List<Long> filterStatusIds = new ArrayList<>();
        if (!CollectionUtils.isEmpty(dtos) ) {
            filterStatusIds.addAll(dtos.stream().map(LinkIssueStatusLinkageDTO::getLinkIssueStatusId).collect(Collectors.toList()));
        }
        if(!CollectionUtils.isEmpty(filterStatusIds)){
            statusVOS = statusVOS.stream().filter(v -> !filterStatusIds.contains(v.getId())).collect(Collectors.toList());
        }
        return statusVOS;
    }

    @Override
    public Map<Long, IssueStatusLinkageVO> queryMapByProject(Long projectId, Long organizationId) {
        LinkIssueStatusLinkageDTO linkageDTO = new LinkIssueStatusLinkageDTO();
        linkageDTO.setProjectId(projectId);
        linkageDTO.setOrganizationId(organizationId);
        List<LinkIssueStatusLinkageDTO> linkageDTOS = linkIssueStatusLinkageMapper.select(linkageDTO);
        if (CollectionUtils.isEmpty(linkageDTOS)) {
            return new HashMap<>();
        }
        List<StatusVO> statusVOS = projectConfigService.queryStatusByProjectId(projectId, SchemeApplyType.AGILE);
        Map<Long, StatusVO> statusMap = new HashMap<>();
        if (!CollectionUtils.isEmpty(statusVOS)) {
            statusMap.putAll(statusVOS.stream().collect(Collectors.toMap(StatusVO::getId, Function.identity())));
        }
        // 获取项目的问题类型
        List<IssueTypeVO> issueTypeVOS = projectConfigService.queryIssueTypesByProjectId(projectId, SchemeApplyType.AGILE, false);
        Map<Long, IssueTypeVO> typeVOMap = new HashMap<>();
        if (!CollectionUtils.isEmpty(statusVOS)) {
            typeVOMap.putAll(issueTypeVOS.stream().collect(Collectors.toMap(IssueTypeVO::getId, Function.identity())));
        }

        List<IssueLinkTypeDTO> issueLinkTypeDTOS = issueLinkTypeMapper.queryIssueLinkTypeByProjectId(projectId, null, null, null);
        Map<Long, IssueLinkTypeVO> issueLinkTypeVOMap = new HashMap<>();
        if (!CollectionUtils.isEmpty(issueLinkTypeDTOS)) {
            List<IssueLinkTypeVO> issueLinkTypeVOS = modelMapper.map(issueLinkTypeDTOS, new TypeToken<List<IssueLinkTypeVO>>() {
            }.getType());
            issueLinkTypeVOMap.putAll(issueLinkTypeVOS.stream().collect(Collectors.toMap(IssueLinkTypeVO::getLinkTypeId, Function.identity())));
        }
        Map<Long, IssueStatusLinkageVO> map = new HashMap<>();
        for (LinkIssueStatusLinkageDTO dto : linkageDTOS) {
            IssueStatusLinkageVO linkageVO = modelMapper.map(dto, IssueStatusLinkageVO.class);
            linkageVO.setLinkageIssueTypeId(dto.getLinkIssueTypeId());
            linkageVO.setLinkageIssueStatusId(dto.getLinkIssueStatusId());
            linkageVO.setLinkTypeVO(issueLinkTypeVOMap.getOrDefault(dto.getLinkTypeId(), null));
            linkageVO.setLinkageIssueType(typeVOMap.getOrDefault(dto.getLinkIssueTypeId(), null));
            linkageVO.setIssueTypeVO(typeVOMap.getOrDefault(dto.getIssueTypeId(), null));
            linkageVO.setStatusVO(statusMap.getOrDefault(dto.getStatusId(), null));
            linkageVO.setLinkageIssueStatus(statusMap.getOrDefault(dto.getLinkIssueStatusId(), null));
            map.put(dto.getId(), linkageVO);
        }
        return map;
    }

    private Set<Long> changeStatus(Long projectId, Long organizationId, String applyType, Map<Long, Map<Long, Long>> issueChangeMap, List<Long> issueIds) {
        Set<Long> updatedIssueIds = new HashSet<>();
        List<IssueDTO> issueDTOS = issueMapper.listIssueInfoByIssueIds(projectId, issueIds, null);
        Map<Long, IssueDTO> issueDTOMap = issueDTOS.stream().collect(Collectors.toMap(IssueDTO::getIssueId, Function.identity()));
        for (Map.Entry<Long, Map<Long, Long>> entry : issueChangeMap.entrySet()) {
            Long issueTypeId = entry.getKey();
            // 获取当前状态对应的transformId
            Long stateMachineId = projectConfigService.queryStateMachineId(projectId, applyType, issueTypeId);
            Map<Long, Long> value = entry.getValue();
            for (Map.Entry<Long, Long> issueEntry : value.entrySet()) {
                Long issueId = issueEntry.getKey();
                Long changeStatus = issueEntry.getValue();
                IssueDTO issueDTO = issueDTOMap.getOrDefault(issueId, null);
                Boolean isSubBug = !ObjectUtils.isEmpty(issueDTO) && (!ObjectUtils.isEmpty(issueDTO.getRelateIssueId()) && issueDTO.getRelateIssueId() != 0L);
                if (ObjectUtils.isEmpty(issueDTO) || Objects.equals(changeStatus, issueDTO.getStatusId()) || isSubBug) {
                    continue;
                }
                // 获取开始状态和结束状态查询转换Id
                List<StatusMachineTransformDTO> statusMachineTransformDTOS = statusMachineTransformMapper
                        .selectTransformByStatusId(organizationId, stateMachineId, issueDTO.getStatusId(), changeStatus, false);
                if (CollectionUtils.isEmpty(statusMachineTransformDTOS)) {
                    statusMachineTransformDTOS = statusMachineTransformMapper
                            .selectTransformByStatusId(organizationId, stateMachineId, issueDTO.getStatusId(), changeStatus, true);
                }
                StatusMachineTransformDTO statusTransform = statusMachineTransformDTOS.get(0);
                issueService.updateIssueStatus(projectId, issueId, statusTransform.getId(),
                        issueDTO.getObjectVersionNumber(), applyType, issueDTO, false);
                updatedIssueIds.add(issueId);
            }
        }
        return updatedIssueIds;
    }
}
