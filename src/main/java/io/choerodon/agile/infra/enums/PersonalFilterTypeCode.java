package io.choerodon.agile.infra.enums;

import java.util.Arrays;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.springframework.util.Assert;

/**
 * @author huaxin.deng@hand-china.com 2021-11-16 16:38:07
 */
public enum PersonalFilterTypeCode {
    /**
     * 工作项个人筛选
     */
    AGILE_ISSUE("agile_issue"),

    /**
     * 工作台工时日历个人筛选
     */
    AGILE_WORK_HOURS("agile_work_hours"),
    /**
     * 工作台工作项工时个人筛选
     */
    WORK_BENCH_WORK_HOURS_ISSUE("work_bench_work_hours_issue"),
    /**
     * 工作台工时日志个人筛选
     */
    WORK_BENCH_WORK_HOURS_LOG("work_bench_work_hours_log"),
    /**
     * 工时日历个人筛选
     */
    WORK_HOURS_CALENDAR("work_hours_calendar"),
    /**
     * 工作项工时个人筛选
     */
    WORK_HOURS_ISSUE("work_hours_issue"),
    /**
     * 工时日志个人筛选
     */
    WORK_HOURS_LOG("work_hours_log"),
    /**
     * 风险个人筛选
     */
    RISK_ISSUE("risk_issue"),
    /**
     * 特性个人筛选
     */
    FEATURE_ISSUE("feature_issue"),
    /**
     * 瀑布个人筛选
     */
    WATERFALL_ISSUE("waterfall_issue"),
    /**
     * 问题分析
     */
    ISSUE_ANALYSIS("issue_analysis"),
    /**
     * 缺陷分析
     */
    BUG_ANALYSIS("bug_analysis"),
    /**
     * 需求分析
     */
    BACKLOG_ANALYSIS("backlog_analysis"),
    /**
     * 需求池
     */
    BACKLOG("backlog"),
    ;

    /**
     * 错误消息: 无效的TypeCode
     */
    private static final String ERROR_TYPE_CODE_ILLEGAL = "error.personalFilter.typeCode.illegal";
    /**
     * 错误消息: TypeCode不能为空
     */
    private static final String ERROR_TYPE_CODE_NOT_NULL = "error.personalFilter.typeCode.notNull";

    /**
     * 所有的TypeCode
     */
    private static final Set<String> ALL_TYPE_CODES = Arrays.stream(values()).map(PersonalFilterTypeCode::getTypeCode).collect(Collectors.toSet());
    /**
     * 所有的TypeCode
     */
    private static final Map<String, PersonalFilterTypeCode> CODE_TO_ENUM_MAP = Arrays.stream(values()).collect(Collectors.toMap(PersonalFilterTypeCode::getTypeCode, Function.identity()));

    private PersonalFilterTypeCode(String typeCode) {
        this.typeCode = typeCode;
    }

    /**
     * 校验TypeCode是否合法, 不合法报错
     * @param personalFilterTypeCode personalFilterTypeCode
     */
    public static void checkTypeCodeValidOrThrow(String personalFilterTypeCode) {
        Assert.hasText(personalFilterTypeCode, ERROR_TYPE_CODE_NOT_NULL);
        Assert.isTrue(ALL_TYPE_CODES.contains(personalFilterTypeCode), ERROR_TYPE_CODE_ILLEGAL);
    }

    /**
     * 根据TypeCode查询枚举
     * @param personalFilterTypeCode personalFilterTypeCode
     * @return 枚举
     */
    public static PersonalFilterTypeCode of(String personalFilterTypeCode) {
        checkTypeCodeValidOrThrow(personalFilterTypeCode);
        return CODE_TO_ENUM_MAP.get(personalFilterTypeCode);
    }

    /**
     * 个人筛选TypeCode
     */
    private final String typeCode;

    /**
     * @return 个人筛选TypeCode
     */
    public String getTypeCode() {
        return typeCode;
    }
}
