package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.AppServiceRepVO;
import io.choerodon.agile.api.vo.AppVersionVO;
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
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

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


    @Override
    public List<AppVersionVO> parse(String inputGroupIds,
                                    InputStream inputStream,
                                    List<AppServiceRepVO> appServiceRepList,
                                    Long organizationId)
            throws ParserConfigurationException, IOException, SAXException {
        DocumentBuilder documentBuilder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
        Document document = documentBuilder.parse(inputStream);
        String serviceCode = getValueByTagName(document, ARTIFACT_ID);
        String version = getValueByTagName(document, VERSION);
        Long projectId = fetchProjectId(serviceCode, appServiceRepList);
        if (projectId == null) {
            throw new CommonException("error.illegal.pom.can.not.find.project");
        }
        List<AppVersionVO> result = new ArrayList<>();
        AppVersionVO appVersionVO = new AppVersionVO();
        appVersionVO.setProjectId(projectId);
        appVersionVO.setOrganizationId(organizationId);
        appVersionVO.setServiceCode(serviceCode);
        appVersionVO.setArtifactId(serviceCode);
        appVersionVO.setVersion(version);
        appVersionVO.setAppService(true);
        appVersionVO.setTag(false);
        result.add(appVersionVO);
        NodeList nodeList = document.getElementsByTagName(DEPENDENCIES);
        if (nodeList.getLength() == 0) {
            throw new CommonException("error.illegal.pom.missing.dependencies");
        }
        Node dependencies = nodeList.item(0);
        NodeList childNodes = dependencies.getChildNodes();
        result.addAll(buildAppVersionFromDependency(inputGroupIds, organizationId, serviceCode, projectId, childNodes));
        return result;
    }

    private List<AppVersionVO> buildAppVersionFromDependency(String inputGroupIds,
                                                             Long organizationId,
                                                             String serviceCode,
                                                             Long projectId,
                                                             NodeList childNodes) {
        Set<String> groupIds = new HashSet<>();
        if (!StringUtils.isEmpty(inputGroupIds)) {
            for (String groupId : inputGroupIds.split(",")) {
                groupIds.add(groupId.trim());
            }
        }
        List<AppVersionVO> result = new ArrayList<>();
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
                    AppVersionVO appVersionVO = new AppVersionVO();
                    appVersionVO.setProjectId(projectId);
                    appVersionVO.setOrganizationId(organizationId);
                    appVersionVO.setServiceCode(serviceCode);
                    appVersionVO.setGroupId(groupId);
                    appVersionVO.setArtifactId(artifactId);
                    appVersionVO.setVersion(version);
                appVersionVO.setTag(false);
                appVersionVO.setAppService(false);
                if (groupIds.isEmpty()) {
                    result.add(appVersionVO);
                } else {
                    if (groupIds.contains(groupId)) {
                        result.add(appVersionVO);
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
