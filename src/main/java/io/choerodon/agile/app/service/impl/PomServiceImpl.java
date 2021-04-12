package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.AppServiceRepVO;
import io.choerodon.agile.api.vo.PublishVersionVO;
import io.choerodon.agile.app.service.PomService;
import io.choerodon.core.exception.CommonException;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import java.io.IOException;
import java.io.InputStream;
import java.util.*;

/**
 * @author superlee
 * @since 2021-03-12
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class PomServiceImpl implements PomService {

    private static final String DEPENDENCIES = "dependencies";
    private static final String DEPENDENCY = "dependency";
    private static final String GROUP_ID = "groupId";
    private static final String ARTIFACT_ID = "artifactId";
    private static final String VERSION = "version";
    private static final String PARENT = "parent";


    @Override
    public List<PublishVersionVO> parse(String inputGroupIds,
                                        InputStream inputStream,
                                        List<AppServiceRepVO> appServiceRepList,
                                        PublishVersionVO self)
            throws ParserConfigurationException, IOException, SAXException {
        DocumentBuilder documentBuilder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
        Document document = documentBuilder.parse(inputStream);
        String artifactId = getValueByTagName(document, ARTIFACT_ID);
        String version = getValueByTagName(document, VERSION);
        String groupId = getGroupIdFromParent(document);
        self.setGroupId(groupId);
        self.setArtifactId(artifactId);
        self.setVersion(version);
        String serviceCode =
                Optional.ofNullable(getAppServiceByArtifactId(appServiceRepList, artifactId))
                        .map(AppServiceRepVO::getCode)
                        .orElse(null);
        self.setServiceCode(serviceCode);
        self.setAppService(true);
        List<PublishVersionVO> result = new ArrayList<>();
        NodeList nodeList = document.getElementsByTagName(DEPENDENCIES);
        if (nodeList.getLength() == 0) {
            throw new CommonException("error.illegal.pom.missing.dependencies");
        }
        Node dependencies = nodeList.item(0);
        NodeList childNodes = dependencies.getChildNodes();
        result.addAll(buildAppVersionFromDependency(inputGroupIds, childNodes, appServiceRepList));
        return result;
    }

    private AppServiceRepVO getAppServiceByArtifactId(List<AppServiceRepVO> appServiceRepList, String artifactId) {
        AppServiceRepVO appServiceRepVO = null;
        for (AppServiceRepVO vo : appServiceRepList) {
            if (artifactId.equals(vo.getArtifactId())) {
                appServiceRepVO = vo;
            }
        }
        return appServiceRepVO;
    }

    private String getGroupIdFromParent(Document document) {
        NodeList nodeList = document.getElementsByTagName(PARENT);
        String groupId = null;
        if (nodeList.getLength() != 0) {
            Node parent = nodeList.item(0);
            NodeList childNodes = parent.getChildNodes();
            for (int i = 0; i < childNodes.getLength(); i++) {
                Node child = childNodes.item(i);
                if (GROUP_ID.equals(child.getNodeName())) {
                    groupId = child.getTextContent();
                }
            }
        }
        return groupId;
    }

    private List<PublishVersionVO> buildAppVersionFromDependency(String inputGroupIds,
                                                                 NodeList childNodes,
                                                                 List<AppServiceRepVO> appServiceRepList) {
        Set<String> groupIds = new HashSet<>();
        if (!StringUtils.isEmpty(inputGroupIds)) {
            for (String groupId : inputGroupIds.split(",")) {
                groupIds.add(groupId.trim());
            }
        }
        List<PublishVersionVO> result = new ArrayList<>();
        for (int i = 0; i < childNodes.getLength(); i++) {
            Node dependency = childNodes.item(i);
            if (DEPENDENCY.equals(dependency.getNodeName())) {
                NodeList dependencyChildNodes = dependency.getChildNodes();
                String groupId = null;
                String artifactId = null;
                String version = null;
                for (int j = 0; j < dependencyChildNodes.getLength(); j++) {
                    Node node = dependencyChildNodes.item(j);
                    if (GROUP_ID.equals(node.getNodeName())) {
                        groupId = node.getTextContent();
                    }
                    if (ARTIFACT_ID.equals(node.getNodeName())) {
                        artifactId = node.getTextContent();
                    }
                    if (VERSION.equals(node.getNodeName())) {
                        version = node.getTextContent();
                    }
                }
                PublishVersionVO publishVersionVO = new PublishVersionVO();
                Long projectId = null;
                String serviceCode = null;
                AppServiceRepVO appServiceRepVO = getAppServiceByArtifactId(appServiceRepList, artifactId);
                if (appServiceRepVO != null) {
                    projectId = appServiceRepVO.getProjectId();
                    serviceCode = appServiceRepVO.getCode();
                }
                publishVersionVO.setServiceCode(serviceCode);
                publishVersionVO.setProjectId(projectId);
                publishVersionVO.setGroupId(groupId);
                publishVersionVO.setArtifactId(artifactId);
                publishVersionVO.setVersion(version);
                publishVersionVO.setAppService(false);
                if (groupIds.isEmpty()) {
                    result.add(publishVersionVO);
                } else {
                    if (groupIds.contains(groupId)) {
                        result.add(publishVersionVO);
                    }
                }
            }
        }
        return result;
    }

    private String getValueByTagName(Document document,
                                     String tagName) {
        NodeList pomArtifactIdNodeList = document.getElementsByTagName(tagName);
        if (pomArtifactIdNodeList.getLength() == 0) {
            throw new CommonException("error.illegal.pom.missing." + tagName);
        }
        return pomArtifactIdNodeList.item(0).getTextContent();
    }

    private Long fetchProjectId(String serviceCode, List<AppServiceRepVO> appServiceRepList) {
        if (ObjectUtils.isEmpty(appServiceRepList)) {
            return null;
        }
        for (AppServiceRepVO vo : appServiceRepList) {
            String code = vo.getCode();
            if (serviceCode.equals(code)) {
                return vo.getProjectId();
            }
        }
        return null;
    }
}
