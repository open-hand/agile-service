package io.choerodon.agile.infra.enums;

import java.util.Arrays;
import java.util.List;

/**
 * @author superlee
 * @since 2022-07-11
 */
public class StatusTransferType {

    /**
     * 指定用户
     */
    public static final String SPECIFIER = "specifier";
    /**
     * 其他, 目前只有一种: 子工作项限制配置
     */
    public static final String OTHER = "other";
    /**
     * 按角色
     */
    public static final String ROLE = "role";
    /**
     * 按经办人
     */
    public static final String ASSIGNEE = "assignee";
    /**
     * 按报告人
     */
    public static final String REPORTER = "reporter";
    /**
     * 按参与人
     */
    public static final String PARTICIPANT = "participant";
    /**
     * 按主要负责人
     */
    public static final String MAIN_RESPONSIBLE = "mainResponsible";
    /**
     * 按风险相关方
     */
    public static final String RELATED_PARTIES = "relatedParties";

    public static final List<String> ALL_TYPES =
            Arrays.asList(
                    SPECIFIER,
                    OTHER,
                    ROLE,
                    ASSIGNEE,
                    REPORTER,
                    PARTICIPANT,
                    MAIN_RESPONSIBLE,
                    RELATED_PARTIES);

    public static final List<String> NOT_DEPEND_ON_ISSUE_DETAILS_TYPES =
            Arrays.asList(
                    SPECIFIER,
                    OTHER,
                    ROLE
            );


    public static boolean isSpecifier(String value) {
        return SPECIFIER.equals(value);
    }

    public static boolean isRole(String value) {
        return ROLE.equals(value);
    }

    public static boolean isOther(String value) {
        return OTHER.equals(value);
    }
}
